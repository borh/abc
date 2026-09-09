use std::fs;
use std::sync::{Arc, Barrier};

use ab_artifact_store::{
    AuthenticateErrorKind, authenticate_blob, authenticate_blob_identity, publish_blob,
    write_atomic_summary,
};
use sha2::{Digest, Sha256};
use tempfile::tempdir;

fn identity(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

#[test]
fn publish_and_authenticate_round_trip() {
    let root = tempdir().unwrap();
    let published = publish_blob(root.path(), "json", b"content").unwrap();
    assert_eq!(published.sha256, identity(b"content"));
    assert_eq!(published.bytes, 7);
    assert!(published.locator.ends_with(".json"));
    assert_eq!(
        authenticate_blob(root.path(), &published.locator, &published.sha256, 7).unwrap(),
        b"content"
    );
    assert_eq!(
        authenticate_blob_identity(root.path(), &published.locator, &published.sha256).unwrap(),
        b"content"
    );
}

#[test]
fn authentication_rejects_absolute_and_parent_locators() {
    let root = tempdir().unwrap();
    let outside = root.path().parent().unwrap().join("artifact-store-outside");
    fs::write(&outside, b"outside").unwrap();
    for locator in [outside.to_str().unwrap(), "../artifact-store-outside"] {
        assert_eq!(
            authenticate_blob(root.path(), locator, &identity(b"outside"), 7)
                .unwrap_err()
                .kind(),
            AuthenticateErrorKind::LocatorInvalid
        );
    }
    fs::remove_file(outside).unwrap();
}

#[test]
fn authentication_rejects_length_and_hash_mismatch() {
    let root = tempdir().unwrap();
    let published = publish_blob(root.path(), "json", b"content").unwrap();
    assert_eq!(
        authenticate_blob(root.path(), &published.locator, &published.sha256, 8)
            .unwrap_err()
            .kind(),
        AuthenticateErrorKind::BlobMismatch
    );
    assert_eq!(
        authenticate_blob(root.path(), &published.locator, &identity(b"other"), 7)
            .unwrap_err()
            .kind(),
        AuthenticateErrorKind::BlobMismatch
    );
}

#[test]
fn authentication_classifies_missing_locator_as_unavailable() {
    let root = tempdir().unwrap();
    assert_eq!(
        authenticate_blob(root.path(), "missing.json", &identity(b"content"), 7)
            .unwrap_err()
            .kind(),
        AuthenticateErrorKind::LocatorUnavailable
    );
}

#[cfg(unix)]
#[test]
fn authentication_rejects_symlink_ancestor_and_destination() {
    use std::os::unix::fs::symlink;

    let root = tempdir().unwrap();
    let outside = tempdir().unwrap();
    fs::write(outside.path().join("blob"), b"content").unwrap();
    symlink(outside.path(), root.path().join("linked")).unwrap();
    assert!(authenticate_blob(root.path(), "linked/blob", &identity(b"content"), 7).is_err());

    fs::create_dir(root.path().join("inside")).unwrap();
    fs::write(root.path().join("inside/blob"), b"content").unwrap();
    symlink(root.path().join("inside"), root.path().join("inside-link")).unwrap();
    assert!(authenticate_blob(root.path(), "inside-link/blob", &identity(b"content"), 7).is_err());

    fs::write(root.path().join("regular"), b"content").unwrap();
    symlink(root.path().join("regular"), root.path().join("destination")).unwrap();
    assert!(authenticate_blob(root.path(), "destination", &identity(b"content"), 7).is_err());
}

#[test]
fn preexisting_collision_is_rejected() {
    let root = tempdir().unwrap();
    let published = publish_blob(root.path(), "json", b"content").unwrap();
    fs::write(root.path().join(&published.locator), b"corrupt").unwrap();
    assert!(publish_blob(root.path(), "json", b"content").is_err());
}

#[cfg(unix)]
#[test]
fn publication_rejects_symlink_ancestor_and_destination() {
    use std::os::unix::fs::symlink;

    let root = tempdir().unwrap();
    let published = publish_blob(root.path(), "json", b"content").unwrap();
    let destination = root.path().join(&published.locator);
    let outside = root.path().join("outside");
    fs::write(&outside, b"content").unwrap();
    fs::remove_file(&destination).unwrap();
    symlink(&outside, &destination).unwrap();
    assert!(publish_blob(root.path(), "json", b"content").is_err());

    fs::remove_dir_all(root.path().join("sha256")).unwrap();
    let outside_dir = tempdir().unwrap();
    symlink(outside_dir.path(), root.path().join("sha256")).unwrap();
    assert!(publish_blob(root.path(), "json", b"other").is_err());
}

#[test]
fn concurrent_identical_publishers_accept_one_shared_winner() {
    let root = Arc::new(tempdir().unwrap());
    let barrier = Arc::new(Barrier::new(8));
    let threads = (0..8)
        .map(|_| {
            let root = Arc::clone(&root);
            let barrier = Arc::clone(&barrier);
            std::thread::spawn(move || {
                barrier.wait();
                publish_blob(root.path(), "json", b"content")
            })
        })
        .collect::<Vec<_>>();
    let publications = threads
        .into_iter()
        .map(|thread| thread.join().unwrap().unwrap())
        .collect::<Vec<_>>();
    assert!(publications.windows(2).all(|pair| pair[0] == pair[1]));
    let publication = &publications[0];
    assert_eq!(
        fs::read(root.path().join(&publication.locator)).unwrap(),
        b"content"
    );
}

#[test]
fn atomic_summary_replaces_complete_previous_value() {
    let root = tempdir().unwrap();
    let path = root.path().join("summary.json");
    write_atomic_summary(&path, b"old").unwrap();
    write_atomic_summary(&path, b"new summary").unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"new summary");
    assert_eq!(fs::read_dir(root.path()).unwrap().count(), 1);
}

#[test]
fn concurrent_summary_readers_observe_only_complete_values() {
    let root = Arc::new(tempdir().unwrap());
    let path = root.path().join("summary.json");
    let old = vec![b'a'; 64 * 1024];
    let new = vec![b'b'; 96 * 1024];
    write_atomic_summary(&path, &old).unwrap();
    let reader_path = path.clone();
    let old_for_reader = old.clone();
    let new_for_reader = new.clone();
    let barrier = Arc::new(Barrier::new(2));
    let reader_barrier = Arc::clone(&barrier);
    let reader = std::thread::spawn(move || {
        reader_barrier.wait();
        for _ in 0..200 {
            let observed = fs::read(&reader_path).unwrap();
            assert!(observed == old_for_reader || observed == new_for_reader);
        }
    });
    barrier.wait();
    for value in [&new, &old] {
        for _ in 0..20 {
            write_atomic_summary(&path, value).unwrap();
        }
    }
    reader.join().unwrap();
}

#[cfg(unix)]
#[test]
fn authentication_classifies_open_permission_failure_as_read_failed() {
    use std::os::unix::fs::PermissionsExt;

    let root = tempdir().unwrap();
    let path = root.path().join("unreadable.json");
    fs::write(&path, b"content").unwrap();
    fs::set_permissions(&path, fs::Permissions::from_mode(0o000)).unwrap();
    let result = authenticate_blob(root.path(), "unreadable.json", &identity(b"content"), 7);
    fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
    assert_eq!(
        result.unwrap_err().kind(),
        AuthenticateErrorKind::ReadFailed
    );
}

#[test]
fn summary_replacement_is_complete_and_uses_new_file_metadata() {
    let root = tempdir().unwrap();
    let path = root.path().join("summary.json");
    write_atomic_summary(&path, b"old").unwrap();
    let old_identity = fs::metadata(&path).unwrap();
    write_atomic_summary(&path, b"new").unwrap();
    let new_identity = fs::metadata(&path).unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"new");
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        assert_ne!(old_identity.ino(), new_identity.ino());
    }
}

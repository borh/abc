# ADR evidence component-root resolution follow-up

The current all-descriptor recapture requires an excluded local `abc/abc -> .`
symlink because component-root profiles resolve one path relative to the
component and another relative to the workspace. The link is a temporary,
pre-existing workaround and is not part of parser-RQ evidence identity.

A later focused change must characterize the existing input-key projection,
make the capture tool resolve component-root and workspace-root without a
self-referential symlink, migrate affected descriptors atomically, and delete
the workaround from runbooks. This portability slice does not combine that
capture-tool correction with the storage-contract rotation.

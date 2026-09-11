# The boot image for the offline key ceremony in docs/key-ceremony.md. The
# installer's minimal CD profile supplies the bootable image; this module
# adds the signing toolchain and removes every path to a network.
{
  lib,
  modulesPath,
  pkgs,
  ...
}:
{
  imports = [ "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

  # The whole toolchain the ceremony uses. cryptsetup is already in the
  # installer's base profile; the openssl CLI is not, and an offline machine
  # cannot fetch it. e2fsprogs is named explicitly so the image does not
  # depend on mkfs.ext4 arriving through some other package's closure.
  environment.systemPackages = [
    pkgs.openssl
    pkgs.cryptsetup
    pkgs.e2fsprogs
  ];

  # This image installs nothing and reaches nothing. The network options need
  # mkForce rather than a plain false: the installer's network stack defines
  # them at normal priority, so an ordinary definition fails evaluation with a
  # conflicting-definition error rather than taking effect.
  services.openssh.enable = false;
  networking.networkmanager.enable = lib.mkForce false;
  networking.wireless.enable = lib.mkForce false;

  # The ISO builder in this nixpkgs derives the file name from baseName and
  # does not read image.fileName, so the base name is what names the file.
  # The installer profile defines it at normal priority, hence mkForce.
  image.baseName = lib.mkForce "snh-ceremony";
  system.stateVersion = "26.11";
}

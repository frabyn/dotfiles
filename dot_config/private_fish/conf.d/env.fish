# Environment variables
set -gx GNUPGHOME $XDG_DATA_HOME/openpgp
set -gx NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
set -gx NPM_CONFIG_CACHE $XDG_CACHE_HOME/npm
set -gx ELECTRON_OZONE_PLATFORM_HINT wayland

# TeXLive
set -gx TEXDIR $XDG_DATA_HOME/texlive/2025
set -gx TEXMFLOCAL $XDG_DATA_HOME/texlive/texmf-local
set -gx TEXMFSYSCONFIG $XDG_CONFIG_HOME/texlive/texmf-config
set -gx TEXMFSYSVAR $XDG_CACHE_HOME/texlive/texmf-var
set -gx TEXMFHOME $XDG_DATA_HOME/texmf
set -gx TEXMFCONFIG $XDG_CONFIG_HOME/texlive/texmf-config
set -gx TEXMFVAR $XDG_CACHE_HOME/texlive/texmf-var

# Rust/Cargo
set -gx CARGO_HOME $XDG_DATA_HOME/cargo
set -gx RUSTUP_HOME $XDG_DATA_HOME/rustup

# UV
set -gx UV_CACHE_DIR $XDG_DATA_HOME/uv

# Ansible
set -gx ANSIBLE_CONFIG $XDG_CONFIG_HOME/ansible/ansible.cfg

# AWS
set -gx AWS_CONFIG_FILE $XDG_CONFIG_HOME/aws/config
set -gx AWS_SHARED_CREDENTIALS_FILE $XDG_DATA_HOME/aws/credentials

# Rootless Podman
set -gx DOCKER_HOST "unix:///run/user/$(id -u)/podman/podman.sock"

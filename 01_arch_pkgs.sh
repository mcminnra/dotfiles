#!/bin/bash

# === Install Packages ===
# This is supposed to be a "core" set of packages I rely on. Not an exaustive set.

ARCH_PKGS=(
    "alacritty"                   # Terminal
    "bat"                         # Better cat replacement
    "btop"                        # Better top replacement
    "emacs-wayland"               # Emacs (for wayland)
    "fastfetch"                   # Neofetch clone
    "fish"                        # Shell
    "github-cli"                  # github-cli
    "git"                         # Git
    "git-delta"                   # Better CLI git deltas
    "libreoffice-fresh"           # Office suite
    "lsd"                         # Better ls
    "rsync"                       # Rsync
    "steam"                       # Steam
    "stow"                        # Gnu Symlink Manager
    "syncthing"                   # P2P file syncing
    "tailscale"                   # Tailscale VPN   
    "tmux"                        # Term Multiplex
    "ttf-sourcecodepro-nerd"      # Source code pro nerd font
    "tree"                        # CLI File Trees
)

AUR_PKGS=(
    "google-chrome"               # Google Chrome
    "oh-my-posh-bin"              # Prompt engine (pretty shells!)\
    "proton-mail-bin"             # Proton mail
    "proton-pass-bin"             # Proton password manager
    "sunshine"                    # Streaming server for Moonlight
    "visual-studio-code-bin"      # IDE
)

# Check for yay
command -v yay &> /dev/null || { echo "yay is not available. Please install."; exit 1; }

echo "Synchronizing Pacman database..."
yay -Sy || { echo "Error: Failed to synchronize Pacman database. Exiting."; exit 1; }
echo "Pacman database synchronized."

echo "Installing Arch packages: ${ARCH_PKGS[*]}"
yay -S --needed --noconfirm "${ARCH_PKGS[@]}" || { echo "Error: Failed to install Arch packages. Exiting."; exit 1; }
echo "Arch packages installed successfully."

echo "Installing AUR packages: ${AUR_PKGS[*]}"
yay -S --needed --noconfirm "${AUR_PKGS[@]}" || { echo "Error: Failed to install AUR packages. Exiting."; exit 1; }
echo "AUR packages installed successfully."

# --- Enable services ---
# TODO

# Example
# if ! systemctl is-enabled NetworkManager &>/dev/null; then
#     echo "Enabling NetworkManager..."
#     sudo systemctl enable NetworkManager || { echo "Warning: Failed to enable NetworkManager. Please check manually."; }
#     sudo systemctl start NetworkManager || { echo "Warning: Failed to start NetworkManager. Please check manually."; }
# else
#     echo "NetworkManager is already enabled."
# fi
#!/bin/bash

# --- Make sure yay installed ---
# TODO

# --- Pkg Install ---
ARCH_PKGS=(
    "alacritty"      # Terminal
    "emacs-wayland"  # Emacs (for wayland)
    # TODO MORE
)

AUR_PKGS=(
    "google-chrome"  # Google Chrome
    # TODO MORE
)
    

echo "Synchronizing Pacman database..."
yay -Sy || { echo "Error: Failed to synchronize Pacman database. Exiting."; exit 1; }
echo "Pacman database synchronized."

echo "Installing Arch packages: ${ARCH_PKGS[*]}"
yay -S --noconfirm "${ARCH_PKGS[@]}" || { echo "Error: Failed to install Arch packages. Exiting."; exit 1; }
echo "Arch packages installed successfully."

echo "Installing AUR packages: ${AUR_PKGS[*]}"
yay -S --noconfirm "${AUR_PKGS[@]}" || { echo "Error: Failed to install AUR packages. Exiting."; exit 1; }
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
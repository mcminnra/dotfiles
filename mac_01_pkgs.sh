#!/bin/bash

# === Intall Packages ===
PKGS=(
    "bat"          # Better cat replacement
    "btop"         # Better top replacement
    "fastfetch"    # Neofetch clone
    "fish"         # Shell
    "gh"           # Github CLI
    "git"          # Git
    "git-delta"    # Better CLI git deltas
    "lsd"          # Better ls
    "oh-my-posh"   # Prompt engine (pretty shells!)
    "rsync"        # Rsync
    "stow"         # Gnu Symlink Manager
    "tmux"         # Term Multiplex
    "tree"         # CLI File Trees
)

CASK_PKGS=(
    "alacritty"                      # Terminal
    "emacs-mac"                      # Editor
    "font-sauce-code-pro-nerd-font"  # Source code pro nerd font
    "google-chrome"                  # Browser
    "libreoffice"                    # Office suite
    "proton-mail"                    # Proton mail
    "proton-pass"                    # Proton password manager
    "spotify"                        # Spotify
    "stats"                          # Top bar system stats
    "syncthing"                      # P2P file syncing
    "tailscale"                      # Tailscale VPN
    "visual-studio-code"             # IDE
)

# Check for homebrew
command -v brew &> /dev/null || { echo "Homebrew is not available. Please install."; exit 1; }

# Update homebrew and upgrade any packages
brew update && brew upgrade

# Install
echo "Installing brew packages: ${PKGS[*]}"
brew install "${PKGS[@]}" || { echo "Error: Failed to install brew packages. Exiting."; exit 1; }
echo "Brew packages installed successfully."

echo "Installing brew cask packages: ${CASK_PKGS[*]}"
brew install --cask "${CASK_PKGS[@]}" || { echo "Error: Failed to install brew cask packages. Exiting."; exit 1; }
echo "Brew cask packages installed successfully."

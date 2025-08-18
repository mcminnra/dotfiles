#!/bin/bash

stow alacritty -t ~/ --verbose=2
stow emacs -t ~/ --verbose=2
stow fish -t ~/ --verbose=2
stow nvim -t ~/ --verbose=2

# === Condionally ask for some folders
# git
read -p "Do you want to stow git? (y/n): " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
    stow git -t ~/ --verbose=2
    case "$OSTYPE" in
        msys|cygwin|win32)
            stow git-windows -t ~/ --verbose=2
            ;;
        darwin*)
            stow git-macos -t ~/ --verbose=2
            ;;
        linux*)
            stow git-linux -t ~/ --verbose=2
            ;;
    esac
fi
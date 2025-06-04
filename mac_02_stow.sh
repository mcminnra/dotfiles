#!/bin/bash

stow alacritty -t ~/ --verbose=2
stow emacs -t ~/ --verbose=2
stow fish -t ~/ --verbose=2

# Condionally ask for some folders
read -p "Do you want to stow git? (y/n): " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
    stow git -t ~/ --verbose=2
fi
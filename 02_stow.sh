#!/bin/bash

stow alacritty -t ~/ --verbose=2
stow claude -t ~/ --verbose=2
stow emacs -t ~/ --verbose=2
stow fish -t ~/ --verbose=2
stow ghostty -t ~/ --verbose=2
stow nvim -t ~/ --verbose=2
stow tmux -t ~/ --verbose=2
stow zed -t ~/ --verbose=2

# Personal vs Work Stows
read -p "Work or Personal? (w/p): " answer
if [[ "$answer" =~ ^[Ww]$ ]]; then
    # Keep work claude stuff out of git
    :
else
    stow claude-personal -t ~/ --verbose=2
fi

# git (by os)
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


#!/usr/bin/bash

ln -sf ~/dotfiles/.gitconfig ~/.gitconfig
echo "Symlinked .gitconfig"

ln -sf ~/dotfiles/.bashrc ~/.bashrc
echo "Symlinked .bashrc"

ln -sf ~/dotfiles/.bash_aliases ~/.bash_aliases
echo "Symlinked .bash_aliases"

ln -sf ~/dotfiles/.bash_profile ~/.bash_profile
echo "Symlinked .bash_profile"

ln -sf ~/dotfiles/.emacs.d ~/.emacs.d
echo "Symlinked .emacs.d"

ln -sf ~/dotfiles/.config/Code ~/.config/Code
echo "Symlinked VSCode .config"
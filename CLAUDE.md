# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles repository using GNU Stow for cross-platform configuration management. Supports macOS (Homebrew) and Arch Linux (Pacman/AUR).

## Setup Commands

```bash
# Install packages (run appropriate one for your OS)
./01_mac_pkgs.sh      # macOS - uses Homebrew
./01_arch_pkgs.sh     # Arch Linux - uses pacman/AUR

# Deploy configurations via GNU Stow
./02_stow.sh          # Symlinks all configs to home directory
```

## Architecture

**GNU Stow Structure**: Each directory is a stow package containing files in their target location relative to `$HOME`:
- `emacs/.config/emacs/` → `~/.config/emacs/`
- `fish/.config/fish/` → `~/.config/fish/`
- `git/.gitconfig` → `~/.gitconfig`

**Platform-Specific Configs**: Git uses conditional includes (`includeIf`) to load `git-macos/` or `git-linux/` for platform-specific settings.

## Key Configurations

| Tool | Config Location | Notes |
|------|-----------------|-------|
| Emacs | `emacs/.config/emacs/init.el` | Uses straight.el for packages |
| Neovim | `nvim/.config/nvim/init.lua` | Uses lazy.nvim for plugins |
| Fish | `fish/.config/fish/config.fish` | Platform-aware PATH setup |
| Zed | `zed/.config/zed/settings.json` | Recently added |
| Alacritty | `alacritty/.config/alacritty/alacritty.toml` | Terminal config |
| oh-my-posh | `oh-my-posh/omp.json` | Prompt theme |

## Conventions

- All configs follow XDG Base Directory spec (`~/.config/`)
- Shell aliases prefer modern replacements: `lsd` (ls), `bat` (cat), `btop` (top), `fd` (find)
- `old/` directory contains deprecated configurations (Doom Emacs, etc.)

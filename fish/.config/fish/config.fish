if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Disable the greeting
set fish_greeting ""

# Set paths
switch (uname -s)
    case Darwin
        fish_add_path /opt/homebrew/bin
        fish_add_path ~/.emacs.d/bin
        fish_add_path ~/.local/bin
        fish_add_path ~/.cargo/bin/  # Rust
    case Linux
        fish_add_path ~/.config/emacs/bin/
        fish_add_path ~/.local/bin/
        fish_add_path ~/.cargo/bin/  # Rust
    case '*'
       echo "Unable to set paths"
end

#== Universal Variables
set -U EDITOR emacs

#== Aliases
alias ls lsd  # Replace ls - https://github.com/lsd-rs/lsd
alias ll "lsd -Al" # Replace ls - https://github.com/lsd-rs/lsd
alias la "lsd -Al" # Replace ls - https://github.com/lsd-rs/lsd
alias cat bat  # Replace cat - https://github.com/sharkdp/bat
alias top btop  # Replace top - https://github.com/aristocratos/btop
alias htop btop  # Replace top - https://github.com/aristocratos/btop
alias find fd  # Replace find - https://github.com/sharkdp/fd
alias sg "s -p google"  # https://github.com/zquestz/s
alias tree "tree -ChD --dirsfirst"
alias ydl youtube-dl  # https://github.com/ytdl-org/youtube-dl
alias neofetch fastfetch  #https://github.com/fastfetch-cli/fastfetch

#== Functions
function org
    set user $HOME
    cd "$HOME/org"
end

# oh-my-posh prompt init
oh-my-posh init fish --config ~/repos/dotfiles/oh-my-posh/omp.json | source

# Manually config Conda
# OSX
if test -f /Users/mcminnra/miniconda3/bin/conda
    eval /Users/mcminnra/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

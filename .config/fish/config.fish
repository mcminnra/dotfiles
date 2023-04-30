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
        alias emacs='emacsclient -t --alternate-editor=nano'  # Sets the terminal emacs experience
    case Linux
        #do linux
    case '*'
       echo "Unable to set paths"
end

#== Universal Variables
set -U EDITOR emacs

#== Aliases
alias ls exa  # Replace ls - https://github.com/ogham/exa
alias ll "exa -alh" # Replace ls - https://github.com/ogham/exa
alias la "exa -alh" # Replace ls - https://github.com/ogham/exa
alias cat bat  # Replace cat - https://github.com/sharkdp/bat
alias top btop  # Replace top - https://github.com/aristocratos/btop
alias htop btop  # Replace top - https://github.com/aristocratos/btop
alias sg "s -p google"  # https://github.com/zquestz/s
alias tree "tree -ChD --dirsfirst"
alias ydl youtube-dl  # https://github.com/ytdl-org/youtube-dl

#== Functions
function db
    set user $HOME
    cd "$HOME/Dropbox"
end

# Starship.rs prompt init
starship init fish | source

# Manually config Conda
# OSX
if test -f /Users/mcminnra/miniconda3/bin/conda
    eval /Users/mcminnra/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end
# Linux
if test -f /home/mcminnra/miniconda3/bin/conda
    eval /home/mcminnra/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

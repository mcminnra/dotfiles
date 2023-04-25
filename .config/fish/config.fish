if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Set paths
switch (uname -s)
    case Darwin
        fish_add_path /opt/homebrew/bin
    case Linux
        #do linux
    case '*'
       echo "Unable to set paths"
end

#== Aliases
alias ls exa
alias cat bat
alias ll "exa -alh"
alias la "exa -alh"

#== Functions
function db
    set user $HOME
    cd "$HOME/Dropbox"
end

# Starship.rs prompt init
starship init fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /Users/mcminnra/miniconda3/bin/conda
    eval /Users/mcminnra/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<


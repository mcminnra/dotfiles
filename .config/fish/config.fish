if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Set paths folder
switch (uname -s)
    case Darwin
        fish_add_path /opt/homebrew/bin
    case Linux
        #do linux
    case '*'
       echo "Unable to set paths"
end

starship init fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /Users/mcminnra/miniconda3/bin/conda
    eval /Users/mcminnra/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<


# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="clean"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(colored-man-pages git python pip web-search zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

alias ll='ls -AlhGrti'
alias lsa='ls -a'
alias gitlog='git log --graph --oneline --all --decorate --pretty="%C(bold)%ad%C(reset) %C(yellow)%h%C(reset) %an %C(blue)%s" --date=format:"%y/%m/%d"'
alias ip='ifconfig | grep -Eo '"'"'inet (addr:)?([0-9]*\.){3}[0-9]*'"'"' | grep -Eo '"'"'([0-9]*\.){3}[0-9]*'"'"' | grep -v '"'"'127.0.0.1'"'"''

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# WELCOME SCREEN
###########################################################################

force_color_prompt=yes
BLACK='\033[0;30m'
BLUE='\033[0;34m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
RED='\033[0;31m'
PURPLE='\033[0;35m'
BROWN='\033[0;33m'
LIGHTGRAY='\033[0;37m'
DARKGRAY='\033[1;30m'
LIGHTBLUE='\033[1;34m'
LIGHTGREEN='\033[1;32m'
LIGHTCYAN='\033[1;36m'
LIGHTRED='\033[1;31m'
LIGHTPURPLE='\033[1;35m'
YELLOW='\033[1;33m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

echo -e "${GREEN}           ,aodObo,                                       "
echo -e "${GREEN}        ,AMMMMP~~~~                                       "
echo -e "${GREEN}     ,MMMMMMMMA.                                          "
echo -e "${GREEN}   ,M;'     'YV'                                          "
echo -e "${GREEN}  AM' ,OMA,                                               "
echo -e "${GREEN} AM|   '~VMM,.      ${LIGHTGREEN}.,ama,____,amma,..                    "
echo -e "${GREEN} MML      )MMMD   ${LIGHTGREEN}.AMMMMMMMMMMMMMMMMMMD.                  "
echo -e "${GREEN} VMMM    .AMMY'  ${LIGHTGREEN},AMMMMMMMMMMMMMMMMMMMMD                  "
echo -e "${GREEN} 'VMM, AMMMV'  ${LIGHTGREEN},AMMMMMMMMMMMMMMMMMMMMMMM,                ${GREEN},"
echo -e "${GREEN}  VMMMMMMV'  ${LIGHTGREEN},AMY~~''  'MMMMMMMMMMMM' '~~             ${GREEN},aMM"
echo -e "${GREEN}  'YMMMM'   ${LIGHTGREEN}AMM'        'VMMMMMMMMP'_              ${GREEN}A,aMMMM"
echo -e "${GREEN}   AMMM'    ${LIGHTGREEN}VMMA. YVmmmMMMMMMMMMMML MmmmY          ${GREEN}MMMMMMM"
echo -e "${GREEN}  ,AMMA   _,${LIGHTGREEN}HMMMMmdMMMMMMMMMMMMMMMML'VMV'         ${GREEN},MMMMMMM"
echo -e "${GREEN}  AMMMA _'MMM${LIGHTGREEN}MMMMMMMMMMMMMMMMMMMMMMMA ''          ${GREEN}MMMMMMMM"
echo -e "${GREEN} ,AMMMMMMMMMM${LIGHTGREEN}MMMMMMMMMMMMMMMMMMMMMMMMa      ,,,   ${GREEN}'MMMMMMM"
echo -e "${GREEN} AMMMMMMMMM'~${LIGHTGREEN}'YMMMMMMMMMMMMMMMMMMMMMMA    ,AMMV    ${GREEN}MMMMMMM"
echo -e "${GREEN} VMV MMMMMV   ${LIGHTGREEN}'YMMMMMMMMMMMMMMMMMMMMMY   'VMMY'  ${GREEN}adMMMMMMM${WHITE}  _    _      _ _   "
echo -e "${GREEN} 'V  MMMM'      ${LIGHTGREEN}'YMMMMMMMV.~~~~~~~~~,aado,'V''   ${GREEN}MMMMMMMMM${WHITE} | |  | |    | | |    "
echo -e "${GREEN}    aMMMMmv       ${LIGHTGREEN}'YMMMMMMMm,    ,/AMMMMMA,      ${GREEN}YMMMMMMMM${WHITE} | |__| | ___| | | ___  "
echo -e "${GREEN}    VMMMMM,,v       ${LIGHTGREEN}YMMMMMMMMMo oMMMMMMMMM'    ${GREEN}a, YMMMMMMM${WHITE} |  __  |/ _ \ | |/ _ \ "
echo -e "${GREEN}    'YMMMMMY'       ${LIGHTGREEN}'YMMMMMMMY' 'YMMMMMMMY     ${GREEN}MMmMMMMMMMM${WHITE} | |  | |  __/ | | (_) |"
echo -e "${GREEN}     AMMMMM  ,        ${LIGHTGREEN}~~~~~,aooooa,~~~~~~      ${GREEN}MMMMMMMMMMM${WHITE} |_|  |_|\___|_|_|\___( )"
echo -e "${GREEN}       YMMMb,d'         ${LIGHTGREEN}dMMMMMMMMMMMMMD,   ${GREEN}a,, AMMMMMMMMMM${WHITE}                      |/  "
echo -e "${GREEN}        YMMMMM, A       ${LIGHTGREEN}YMMMMMMMMMMMMMY   ${GREEN},MMMMMMMMMMMMMMM${WHITE}  __  __        __  __      __  __ _"
echo -e "${GREEN}       AMMMMMMMMM        ${LIGHTGREEN}'~~~~'  '~~~~'   ${GREEN}AMMMMMMMMMMMMMMM${WHITE} |  \/  |      |  \/  |    |  \/  (_)"
echo -e "${GREEN}       'VMMMMMM'  ,A,                  ,,AMMMMMMMMMMMMMMMM${WHITE} | \  / |_ __  | \  / | ___| \  / |_ _ __  _ __"
echo -e "${GREEN}     ,AMMMMMMMMMMMMMMA,       ,aAMMMMMMMMMMMMMMMMMMMMMMMMM${WHITE} | |\/| | '__| | |\/| |/ __| |\/| | | '_ \| '_ \ "
echo -e "${GREEN}   ,AMMMMMMMMMMMMMMMMMMA,    AMMMMMMMMMMMMMMMMMMMMMMMMMMMM${WHITE} | |  | | |_   | |  | | (__| |  | | | | | | | | |"
echo -e "${GREEN} ,AMMMMMMMMMMMMMMMMMMMMMA   AMMMMMMMMMMMMMMMMMMMMMMMMMMMMM${WHITE} |_|  |_|_(_)  |_|  |_|\___|_|  |_|_|_| |_|_| |_|"
echo -e "${GREEN}AMMMMMMMMMMMMMMMMMMMMMMMMAaAMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM${WHITE}                                                "
echo ""

#
# .bashrc
# Ryder McMinn
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Use bash-completion, if available
[[ $PS1 && -f /usr/local/etc/bash-completion ]] && \
    . /usr/local/etc/bash-completion

force_color_prompt=yes

# Colors
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

# Terminal Colors (ls - black background)
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# Color the colon red if root
ROOT="${NC}"
if [ ${UID} -eq 0 ]; then
    ROOT="${RED}"
fi

# Prompt
export PROMPT_COMMAND='q="- $(date +%T)"; while [[ ${#q} -lt $COLUMNS ]]; do q="${q:0:1}$q"; done; echo -e "\033[0;90m$q\033[0;37m";'
export PS1="${WHITE}[ \[${LIGHTGREEN}\]\u ${NC}@ ${GREEN}\h${LIGHTBLUE} \W${NC}]${ROOT} \$ ${NC}"

# Defaults
export EDITOR="emacs"
export TERMINAL="terminator"
#export TERM=xterm-256color

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Colored Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Path Adds
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    if [[ "$USER" == "rymcminn" ]]; then
        # >>> conda initialize >>>
        # !! Contents within this block are managed by 'conda init' !!
        __conda_setup="$('/home/rymcminn/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
        if [ $? -eq 0 ]; then
            eval "$__conda_setup"
        else
            if [ -f "/home/rymcminn/anaconda3/etc/profile.d/conda.sh" ]; then
                . "/home/rymcminn/anaconda3/etc/profile.d/conda.sh"
            else
                export PATH="/home/rymcminn/anaconda3/bin:$PATH"
            fi
        fi
        unset __conda_setup
        # <<< conda initialize <<<
    else
        export PATH=/opt/anaconda/bin:$PATH
    fi
fi

# FUNCTIONS
###########################################################################
# General Extract Function

extract () {
    if [ -f $1 ] ; then
      case $1 in
        *.tar.bz2)   tar xjf $1     ;;
        *.tar.gz)    tar xzf $1     ;;
        *.bz2)       bunzip2 $1     ;;
        *.rar)       unrar e $1     ;;
        *.gz)        gunzip $1      ;;
        *.tar)       tar xf $1      ;;
        *.tbz2)      tar xjf $1     ;;
        *.tgz)       tar xzf $1     ;;
        *.zip)       unzip $1       ;;
        *.Z)         uncompress $1  ;;
        *.7z)        7z x $1        ;;
        *)     echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# ls after a cd
function cd()
{
 builtin cd "$*" && ls
}

# WELCOME SCREEN
###########################################################################

clear

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

#Autostart X
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi


#
# .bash_aliases
# Ryder McMinn
#

# Useful Aliases
alias please='sudo $(history -p !-1)'
alias maze='clear; while true; do (( RANDOM % 2 )) && echo -ne "\e[3$(( $RANDOM % 8 ))m╱" || echo -n ╲; sleep .07; done'
alias gitlog='git log --graph --oneline --all --decorate --pretty="%C(bold)%ad%C(reset) %C(yellow)%h%C(reset) %an %C(blue)%s" --date=format:"%y/%m/%d"'
alias weather='curl wttr.in/~Indiana+University+Bloomington'
alias matrix='cmatrix'
alias mostusedbash='history | awk '"'"'{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}'"'"' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n25'
alias ports='netstat -tulanp'
alias ip='ifconfig | grep -Eo '"'"'inet (addr:)?([0-9]*\.){3}[0-9]*'"'"' | grep -Eo '"'"'([0-9]*\.){3}[0-9]*'"'"' | grep -v '"'"'127.0.0.1'"'"''
alias ipext='curl -s http://checkip.dyndns.org/ | grep -o '[0-9][0-9]*.[0-9][0-9]*.[0-9][0-9]*.[0-9]*''
alias irc='irssi'
alias syncportfolio='rsync -rv ~/Projects/portfolio/* mcminnra@50.116.44.24:/srv/http/rydermcminn.com/public_html'
alias beerme='brew update && brew upgrade && brew cleanup -s && brew doctor && brew missing'

# Common Directories
alias home='cd ~'
alias projects='cd ~/Projects'

# Extensions
alias ll='ls -AlhGrti'
alias lsa='ls -a'
alias ..='cd ..'
alias ....='cd ../..'

# Mistypes
alias ccd='cd'
alias cdd='cd'

# Netctl switchs
alias iu='sudo netctl switch-to iu'
alias mcminn='sudo netctl switch-to McMinn'

# Remote Servers
alias silo='ssh silo.cs.indiana.edu -l rmcminn'
alias bigred2='ssh bigred2.uits.iu.edu -l rmcminn'
alias tank='ssh -v tank.cs.indiana.edu -l rmcminn'
alias raichu='ssh -v raichu.soic.indiana.edu -l rmcminn'
alias blitzle='ssh -v blitzle.soic.indiana.edu -l rmcminn'
alias tynamo='ssh -v -L 16006:127.0.0.1:6006 tynamo.soic.indiana.edu -l rmcminn'
alias rydermcminn.com='ssh 50.116.44.24 -l mcminnra'
alias burrow='ssh burrow.cs.indiana.edu -l rmcminn'
alias xsede='ssh -l rmcminn login.xsede.org'

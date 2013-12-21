# Set up the prompt

autoload -Uz promptinit
autoload -U colors && colors
promptinit

# PROMPT theme
#PROMPT=" λ %{$fg[magenta]%}%n%{$reset_color%} %~ %{$fg[magenta]%}→%{$reset_color%} "
PROMPT=" λ %{$fg[magenta]%}%~%{$reset_color%} → "
PS2=" %{$fg[magenta]%}→%{$reset_color%} "

setopt histignorealldups sharehistory

# Word completion goes both ways
setopt completeinword

# Type `..` instead of `cd ..` to change directory like magic
setopt auto_cd

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -v

# Other key binds
#bindkey "^[[1;5D" backward-word
#bindkey "^[[1;5C" forward-word

# Path for Haskell's cabal installed software
if [ -z $( echo $PATH | grep ".cabal/bin" ) ]; then
    export PATH="$PATH:$HOME/.cabal/bin";
fi

# Editor
EDITOR="vim"

# Keep 10000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt incappendhistory
setopt extendedhistory

# Display CPU stats if command takes > 10 seconds to execute
REPORTTIME=10

# Aliases
# These are useful Fogay
# Debian / APT
alias apti="sudo apt-get install"
alias apts="aptitude search"
alias aptsh="aptitude show"
alias aptr="sudo apt-get remove"
alias aptri="sudo apt-get install --reinstall"
alias aptud="sudo apt-get update"
alias aptug="sudo apt-get upgrade"
alias aptdu="sudo apt-get dist-upgrade"
alias aptbd="sudo apt-get build-dep"
alias aptdl="apt-get download"
alias aptpu="sudo apt-get purge"
alias aptar="sudo apt-get autoremove"
alias aptac="sudo apt-get autoclean"
alias aptc="apt-cache"
alias aptpf="sudo vim /etc/apt/preferences"
alias aptppa="sudo apt-add-repository"
alias ppakey="sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys"

# Fedora / RPM
alias yumi="sudo yum install"
alias yumud="sudo yum update"
alias yumug="sudo yum upgrade"
alias yums="yum search"
alias yumr="sudo yum remove"
alias yumwp="sudo yum whatprovides"

# Gentoo / Portage
alias em="emerge"
alias ems="emerge --search"

# Not related to package management
alias trans7="sleep .8s && transset-df -a"
alias slist="sudo vim /etc/apt/sources.list"
alias nautilus="nautilus --no-desktop"
alias la="ls -ah"
alias ls="ls -h --color=auto"
alias ll="ls -lh"
# If you want rm to move to the trash instead use this, otherwise remove the
# line below and it will act like `rm' should
alias rm="mv --target-directory=$HOME/.local/share/Trash/files/"
alias tmux="tmux -2"
alias ghci="ghci +RTS -M1024m"
alias junzip="UNZIP=\"-O cp932\" unzip"
alias empty-trash="\rm -rf ~/.local/share/Trash/files/*"
alias fuck="sudo"
alias FUCK="sudo"
alias fucking="sudo"
alias FUCKING="sudo"
alias goodnight="shutdown -h now"
alias v="vim"
alias grep="grep --color"
alias mkdir="mkdir -p"
alias s="ssh"
alias :q="exit"
alias shoudo="sudo"

# Use modern completion system
autoload -Uz compinit
fpath=(~/.zsh-completion $fpath)
compinit

# Colors
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Start X if you're logging in from tty1.
if [ $(tty) = /dev/tty1 ]; then
    exec startx
fi

# This launches tmux, a terminal multiplexer (look it up) that allows you to
# split terminals and detach sessions and such. Good if you accidentally close
# a terminal, just do `tmux attach -d -t` and press tab to see which tmux
# sessions are not attached. Also, you might not want to nest tmux sessions
# so if you're going to reattach one, exit the tmux you're in with Ctrl+D first.
if [ "$SSH" = "1" ]; then
    tmux attach -d -t 0
elif [ -z $TMUX ]; then
    tmux
fi

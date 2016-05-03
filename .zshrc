# Set up the prompt

autoload -Uz promptinit
autoload -U colors && colors
promptinit

# PROMPT theme
#PROMPT=" λ %{$fg[magenta]%}%n%{$reset_color%} %~ %{$fg[magenta]%}→%{$reset_color%} "
PROMPT="%{%K{033}%F{white}%} 🖿 %{%f%k%}%{%K{032}%F{white}%} %~ %{%f%k%} "
PS2=" %{$fg_bold[red]%}→%{$reset_color%} "

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

# PATHS
# XXX order matters here

# Path for /opt/ binaries
for d in /opt/*; do
    if [ -d $d ]; then
        for sd in $d/*; do
            if [ -d $sd/bin ]; then
                if [ -z $( echo $PATH | grep "$sd/bin" ) ]; then
                    export PATH="$sd/bin:$PATH";
                fi
            fi
        done
    fi
done

# Path for Haskell's Cabal installed software
if [ -z $( echo $PATH | grep ".cabal/bin" ) ]; then
    export PATH="$HOME/.cabal/bin:$PATH";
fi

# Path for Haskell's Stack installed software
if [ -z $( echo $PATH | grep "/.local/bin" ) ]; then
    export PATH="$HOME/.local/bin:$PATH";
fi

# Path for NIX
if [ -z $( echo $PATH | grep "/.nix-profile/" ) ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
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
alias aptppu="sudo ppa-purge"
alias ppakey="sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys"

# NIX
alias nixi="nix-env -i"
alias nixs="nix-env -qaP"
alias nixsh="nix-env -qa --description"
alias nixr="nix-env -e"
alias nixud="nix-channel --update"
alias nixug="nix-env -u"

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
alias lla="ls -lha"
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
alias sf="sshfs"
alias :q="exit"
alias shoudo="sudo"
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias l="ls"
alias xo="xdg-open"
alias open="xdg-open"

# Use modern completion system
autoload -Uz compinit
fpath=(~/.zsh-completion $fpath)
compinit

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=245'
bindkey '^ ' autosuggest-accept

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


if [ "$SSH" = "1" ]; then
    tmux attach -d -t 0
elif [ -z $TMUX ]; then
        tmux
fi

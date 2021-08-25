typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export QT_QPA_PLATAFORMTHEME="qt5ct"
export PATH
export BAT_THEME="Solarized (dark)"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export TERM="xterm-256color"
export EDITOR="nvim"
source ~/.config/zsh/ls-colors
export LS_COLORS
source /home/gabriel/.config/lf/icons

alias gc='git commit -av'
alias gd='git diff'
alias gg='g++ -std=c++17 -O2 -Wall test.cpp -o test -lm -g'
alias gs='git status'
alias grep='grep --color=auto'
alias ls='ls --color=always'
alias pc='cd ~/Code/PC/'
alias xclip='xclip -sel clip '
alias xmo='cd ~/.xmonad'
alias unb='cd ~/UnB/2021-1/'

autoload -Uz compinit 
compinit

# Enables menu to autocompletion
zstyle ':completion:*' menu select 
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Autocompletion of command line switches for aliases
setopt COMPLETE_ALIASES 

# Small letters will match capital letters
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

bindkey -e    # Emacs mode
# bindkey -v  # Vi mode
# bindkey -M viins 'jk' vi-cmd-mode

SAVEHIST=1000000
HISTFILE=~/.zsh_history
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
    if [ -x "$(command -v nvr)" ]; then
        alias nvim=nvr
    else
        alias nvim='echo "No nesting!"'
    fi
fi

source /home/gabriel/.config/zsh/zsh-plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# source /home/gabriel/Repos/zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# [ -f /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh ] && source /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh

eval "$(starship init zsh)"

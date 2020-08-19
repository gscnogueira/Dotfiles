# author:Gabriel S. C. Nogueira
# e-mail:gab.nog94@gmail.com
# blsu   
# github:https://github.com/nosgueira


#---------------------------------------------------------
#----------------------DEFINITIONS------------------------
#---------------------------------------------------------
typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export PATH
export TERM="xterm-256color"
export EDITOR="nvim"
source ~/.config/zsh/ls-colors
export LS_COLORS
source /home/gabriel/.config/lf/icons
#---------------------------------------------------------
#------------------------ALIASES--------------------------
#---------------------------------------------------------

alias vim='nvim'
alias wk='nvim -c VimwikiIndex'
alias ls='exa --color=always'

#---------------------------------------------------------
#-----------------------SETTINGS------------------------
#---------------------------------------------------------

# Set autocompletion
autoload -Uz compinit 
compinit

# Enables menu to autocompletion
zstyle ':completion:*' menu select 
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"


# Autocompletion of command line switches for aliases
setopt COMPLETE_ALIASES 

# Small letters will match capital letters
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' 

#Emacs mode
bindkey -e 

# Vi mode
# bindkey -v 

# History Search:
SAVEHIST=1000
HISTFILE=~/.zsh_history
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

#---------------------------------------------------------
#-----------------------KEY-BINDINGS----------------------
#---------------------------------------------------------
#
typeset -g -A key

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#---------------------------------------------------------
#---------------------PLUGIN-SETTINGS---------------------
#---------------------------------------------------------

#**************************THEME**************************
POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status)
#**************************SCRIPTS************************

#this one is to avoid nesting inseide nvim terminal:

if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
    if [ -x "$(command -v nvr)" ]; then
        alias nvim=nvr
    else
        alias nvim='echo "No nesting!"'
    fi
fi

# Plugins
source  ~/Repos/zsh-plugins/powerlevel9k/powerlevel9k.zsh-theme
source /home/gabriel/Repos/zsh-plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /home/gabriel/Repos/zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -f /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh ] && source /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh


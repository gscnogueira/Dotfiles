# author: Gabriel S. C. Nogueira
# e-mail: gab.nog94@gmail.com
# github: https://github.com/nosgueira
#
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi



#---------------------------------------------------------
#----------------------DEFINITIONS------------------------
#---------------------------------------------------------
typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export QT_QPA_PLATAFORMTHEME="qt5ct"
export PATH
export BAT_THEME="Nord"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export TERM="xterm-256color"
export EDITOR="nvim"
source ~/.config/zsh/ls-colors
export LS_COLORS
source /home/gabriel/.config/lf/icons

#---------------------------------------------------------
#------------------------ALIASES--------------------------
#---------------------------------------------------------
alias gc='git commit -av'
alias gd='git diff'
alias gg='g++ -std=c++11 -O2 -Wall test.cpp -o test -lm -g'
alias grep='grep --color=auto'
alias gs='git status'
alias ls='ls --color=always'
alias org='cd ~/Documents/Notes'
alias pc='cd ~/Codespace/ProgCmp/'
alias please='echo "ciro é gay"'
alias td='nvim -c VimwikiMakeDiaryNote'
alias tp1='cd ~/Codespace/UnB/TP1'
alias wk='nvim -c VimwikiIndex'
alias xclip='xclip -sel clip <'
alias xmo='cd ~/.xmonad'

#---------------------------------------------------------
#-----------------------SETTINGS------------------------
#---------------------------------------------------------


# Set dir_colors theme
test -r ~/.dir_colors && eval $(dircolors ~/.dir_colors)
# Set auto completion
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
# bindkey -M viins 'jk' vi-cmd-mode

# History Search:
SAVEHIST=1000000
HISTFILE=~/.zsh_history
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

#---------------------------------------------------------
#-----------------------KEY-BINDINGS----------------------
#---------------------------------------------------------

typeset -g -A key

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#---------------------------------------------------------
#---------------------PLUGIN-SETTINGS---------------------
#---------------------------------------------------------

#**************************THEME**************************
# POWERLEVEL9K_MODE='nerdfont-complete'
# POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
# POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status)

#---------------------------------------------------------
#-------------------------PLUGINS-------------------------
#---------------------------------------------------------

# POWERLEVEL10K Theme
source ~/Repos/zsh-plugins/powerlevel10k/powerlevel10k.zsh-theme

# POWERLEVEL9K Theme
# source  ~/Repos/zsh-plugins/powerlevel9k/powerlevel9k.zsh-theme
# Syntax Highlighting
source /home/gabriel/Repos/zsh-plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Auto-Suggestions 
source /home/gabriel/Repos/zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# FZF
[ -f /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh ] && source /home/gabriel/Repos/zsh-plugins/fzf/fzf.zsh

typeset -g POWERLEVEL9K_INSTANT_PROMPT=off
#---------------------------------------------------------
#------------------------SCRIPTS--------------------------
#---------------------------------------------------------

# this one is to avoid nesting inside nvim terminal:
if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
    if [ -x "$(command -v nvr)" ]; then
        alias nvim=nvr
    else
        alias nvim='echo "No nesting!"'
    fi
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
[ -f "/home/gabriel/.ghcup/env" ] && source "/home/gabriel/.ghcup/env" # ghcup-env

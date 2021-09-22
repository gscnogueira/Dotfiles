# Setup fzf
# ---------
if [[ ! "$PATH" == */home/gabriel/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/gabriel/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/gabriel/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/gabriel/.fzf/shell/key-bindings.zsh"

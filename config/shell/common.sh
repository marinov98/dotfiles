###################################
####### EDITOR
###################################

export EDITOR=nvim
export VISUAL=nvim

# emacs option
# export EDITOR="emacsclient -t -a ''"
# export VISUAL="emacsclient -c -a emacs"

###################################
####### Aliases
###################################

alias ls='ls -GFh --color=auto'
alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -pv'
alias ll='ls -l'
alias less='less -FSRXc'

alias v='nvim'
alias vc='v --clean'
alias ec='emacsclient -n -c -a ""'
alias zj='zellij'
alias path='echo -e ${PATH//:/\\n}'

###################################
####### FZF
###################################

CUSTOM_PROJECTS_DIR_PATH="$HOME/projects/"

# Fd
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias vz='v $(fzf)'

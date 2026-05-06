###################################
####### Aliases
###################################

alias ls='ls -GFh --color=auto'
alias ll='ls -l'
alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -pv'
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

# Ripgrep
# export FZF_DEFAULT_COMMAND='rg --files --hidden -g "!{.git}"'
# alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 


# Fd
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias vz='v $(fzf)'


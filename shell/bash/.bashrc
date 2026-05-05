#### History Size
export HISTSIZE=10000
export HISTFILESIZE=120000

export EDITOR=nvim
export VISUAL=nvim

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

CUSTOM_PROJECTS_DIR_PATH="$HOME/projects/" # Change this based on your projects directory

# Ripgrep
# export FZF_DEFAULT_COMMAND='rg --files --hidden -g "!{node_modules,.git}"'
# alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 

# Fd
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias vz='v $(fzf)'

###################################
####### Interactive Check
###################################

# Stop here if not interactive
[[ $- != *i* ]] && return

###################################
####### Bindings
###################################

bind '"\C-f":"vz\n"'
bind '"\C-l":"zfd\n"'
bind '"\C-p":"zfd && vz\n"'

###################################
####### PS1
###################################

_RED=$(tput setaf 1)
_GREEN=$(tput setaf 2)
_YELLOW=$(tput setaf 3)
_BLUE=$(tput setaf 4)
_MAGENTA=$(tput setaf 5)
_CYAN=$(tput setaf 6)
_WHITE=$(tput setaf 7)

## Git integration
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

parse_git_status() {
   git_status="$(git status 2> /dev/null)"
   [[ "$git_status" =~ "Changes to be committed:" ]] && echo -n "${_GREEN} *"
   [[ "$git_status" =~ "Changes not staged for commit:" ]] && echo -n "${_YELLOW} *"
   [[ "$git_status" =~ "Untracked files:" ]] && echo -n "${_RED} *"
   [[ "$git_status" =~ "Your branch is behind" ]] && echo -n "${_RED} ^"
   [[ "$git_status" =~ "Your branch is ahead" ]] && echo -n "${_GREEN} ^"
   [[ "$git_status" =~ "have diverged" ]] && echo -n "${_RED} !"
}

# Git autocompletetion
if [ -f "$HOME/git-completion.bash" ]; then
    source "$HOME/git-completion.bash"
fi

 
export PS1="\[\033[36m\]\u@\h:\033[32m\]\w\[\033[1;35m\]\$(parse_git_branch)\[\033[00m\]\$(parse_git_status)\n\[\033[1;33m\]$ \[\033[00m\]"


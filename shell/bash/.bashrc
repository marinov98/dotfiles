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


###################################
####### Aliases
###################################

alias ls='ls -GFh --color=auto'             # Preferred 'ls' implementation
alias cp='cp -iv'                           # Preferred 'cp' implementation
alias mv='mv -iv'                           # Preferred 'mv' implementation
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
alias ll='ls -l'                            # Preferred alternative 'ls' implementation
alias less='less -FSRXc'                    # Preferred 'less' implementation

alias v='vim'             		              # v:            Opens any file in vim/nvim editor
alias vc='v --clean'                        # vc:           Opens any file in vim/nvim editor without config
alias ec='emacsclient -n -c -a ""'          # ec:           Opens emacs server
alias c='code .'                            # c:            Open VS Code
alias t='tmux'                              # t:            Opens tmux
alias ts='tmux attach'                      # ts:           Tmux attaches to specified session
alias zj='zellij'                           # zj:           Start zellij

alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash

#### History Size
export HISTSIZE=10000
export HISTFILESIZE=120000

###################################
####### FZF
###################################

CUSTOM_PROJECTS_DIR_PATH="$HOME/projects/" # Change this based on your projects directory

# Ripgrep
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore-vcs'
alias fzfi='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}" | fzf'
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 

# Fd
# export FZF_DEFAULT_COMMAND='fd --type f --hidden'
# alias fzfi='fd --type file --hidden --no-ignore --exclude .git | fzf'
# alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'

alias vz='v $(fzfi)'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"


###################################
####### Bindings
###################################

bind '"\C-f":"vz\n"'
bind '"\C-l":"zfd\n"'
bind '"\C-p":"zfd && vz\n"'

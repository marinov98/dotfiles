## THIS IS MEANT FOR MAC 

###################################
####### PS1   
###################################

## Git integration
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

_RED=$(tput setaf 1)
_GREEN=$(tput setaf 2)
_YELLOW=$(tput setaf 3)
_BLUE=$(tput setaf 4)
_MAGENTA=$(tput setaf 5)
_CYAN=$(tput setaf 6)
_WHITE=$(tput setaf 7)

parse_git_status() {
   git_status="$(git status 2> /dev/null)"
   [[ "$git_status" =~ "Changes to be committed:" ]] && echo -n "${_GREEN} *"
   [[ "$git_status" =~ "Changes not staged for commit:" ]] && echo -n "${_YELLOW} *"
   [[ "$git_status" =~ "Untracked files:" ]] && echo -n "${_RED} *"
   [[ "$git_status" =~ "Your branch is behind" ]] && echo -n "${_RED} ^"
}

# Git autocompletetion
source ~/git-completion.bash

 
PS1="\[$(tput bold)\]\n";
PS1+="\[$(tput setaf 39)\]$(whoami) ";        # blue  user
PS1+="\[$(tput setaf 148)\]at: "
PS1+="\[$(tput setaf 196)\]\W";   # red directories
PS1+="\[$(tput setaf 162)\]\$(parse_git_branch) > "; #github integration
PS1+="\[$(tput sgr0)\]\$(parse_git_status)";
PS1+="\[$(tput sgr0)\]";
export PS1;


###################################
####### Aliases
###################################

alias ls='ls -GFh --color=auto'             # Preferred 'ls' implementation
alias cp='cp -iv'                           # Preferred 'cp' implementation
alias mv='mv -iv'                           # Preferred 'mv' implementation
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
alias ll='ls -FGlAhp --color=auto'          # Preferred alternative 'ls' implementation
alias less='less -FSRXc'                    # Preferred 'less' implementation

alias v='vim'             		              # v:            Opens any file in vim/nvim editor
alias vc='v --clean'                        # vc:           Opens any file in vim/nvim editor without config
alias ec='emacsclient -n -c -a ""'          # ec:           Opens emacs server
alias c='code .'                            # c:            Open VS Code
alias rr='ranger'                           # rr:           Opens ranger
alias t='tmux'                              # t:            Opens tmux
alias ts='tmux attach'                      # ts:           Tmux attaches to specified session
alias zj='zellij'                           # zj:           Start zellij

alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up

alias get='sudo apt install'                # get:          UBUNTU: installs specified package(s)
alias rem='sudo apt remove'                 # rem:          UBUNTU: removes specified package(s)        
alias purge='sudo apt purge'                # purge:        UBUNTU: purges specified package(s)
alias p='sudo pacman'                       # p:            ARCH: shorter install/update/remove command

alias which='type -all'                     # which:        Find executables
alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias show_options='shopt'                  # Show_options: display bash options settings
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash

# Fix Vim C-s crash
stty -ixon

#### History Size
export HISTSIZE=10000
export HISTFILESIZE=120000

###################################
####### Powerline-Shell PS1   
###################################

# function _update_ps1() {
#     PS1=$(powerline-shell $?)
# }

# if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#     PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
# fi

neofetch

# FZF customization
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore-vcs'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

CUSTOM_PROJECTS_DIR_PATH="$HOME/Projects/" # Change this based on your projects directory
alias fzfi='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}" | fzf'
alias zfind='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'
alias vz='v $(fzfi)'

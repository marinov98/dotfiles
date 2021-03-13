# Path to your oh-my-zsh installation.
export ZSH="/Users/marinmarinov/.oh-my-zsh"

###################################
####### SETTINGS
###################################
ZSH_THEME="powerlevel10k/powerlevel10k"

CASE_SENSITIVE="true"

export EDITOR=~/nvim-osx64/bin/nvim
export VISUAL=~/nvim-osx64/bin/nvim

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
# export MANPATH="/usr/local/man:$MANPATH"

# export MANPATH="/usr/local/man:$MANPATH"

plugins=(
  git 
  z
  zsh-syntax-highlighting
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

###################################
####### Aliases   
###################################

alias ls='ls -GFh'
alias ls='ls -G'
alias ll='ls -lG'
alias cp='cp -iv'                           # Preferred 'cp' implementation
alias mv='mv -iv'                           # Preferred 'mv' implementation
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
alias ll='ls -FGlAhp'                       # Preferred 'ls' implementation
alias less='less -FSRXc'                    # Preferred 'less' implementation

alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
alias ..='cd ../'                           # Go back 1 directory level
alias ...='cd ../../'                       # Go back 2 directory levels
alias .3='cd ../../../'                     # Go back 3 directory levels
alias .4='cd ../../../../'                  # Go back 4 directory levels
alias .5='cd ../../../../../'               # Go back 5 directory levels
alias .6='cd ../../../../../../'            # Go back 6 directory levels

alias v='~/./nvim-osx64/bin/nvim'             # v:            Opens any file in vim editor
alias ec='emacsclient -n -c -a ""'          # ec:           Opens emacs server
alias c='code .'                            # c:            Open VS Code
alias rr='ranger'                           # rr:           Opens ranger
alias t='tmux'                              # t:            Opens tmux
alias ts='tmux attach'                      # ts:           Tmux attaches to specified session

alias f='open -a Finder ./'                 # f:            Opens current directory in MacOS Finder
alias ~="cd ~"                              # ~:            Go Home
alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias show_options='shopt'                  # Show_options: display bash options settings
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up
alias cic='set completion-ignore-case On'   # cic:          Make tab-completion case-insensitive

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash

# Fix Vim C-s crash
stty -ixon

# FZF customization
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore-vcs'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

alias fzfi='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}" | fzf'
alias vz='v $(fzfi)'

# Path to your oh-my-zsh installation.
export ZSH="/Users/marinmarinov/.oh-my-zsh"

###################################
####### SETTINGS
###################################
ZSH_THEME="robbyrussell" # "powerlevel10k/powerlevel10k" if we are feeling extra

CASE_SENSITIVE="true"

export TERM="xterm-256color" 

# vim/nvim option
export EDITOR=vim # change to nvim if using neovim
export VISUAL=vim

# emacs option
# export EDITOR="emacsclient -t -a ''"
# export VISUAL="emacsclient -c -a emacs"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
# export MANPATH="/usr/local/man:$MANPATH"

# export MANPATH="/usr/local/man:$MANPATH"

plugins=(
  git
  zsh-syntax-highlighting
  zsh-autosuggestions
  vi-mode
)

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash

# Fix Vim C-s crash
stty -ixon

# FZF customization
# export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore-vcs' # ripgrep variant
export FZF_DEFAULT_COMMAND='fd --type f --hidden --no-ignore'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

CUSTOM_PROJECTS_DIR_PATH="$HOME/Projects/" # Change this based on your projects directory
alias fzfi='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}" | fzf'
alias zfind='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'
alias vz='v $(fzfi)'

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#6272a4'
ZSH_HIGHLIGHT_STYLES[path]='fg=#8be9fd'

if which fd >/dev/null; then
  goto-projects() {
    zfd
  }
else
  goto-projects() {
    zfind
  }
fi


rg-find-file() {
  vz
}

goto-projects-and-find() {
  zle goto-projects
  zle rg-find-file
}

zle -N goto-projects
zle -N rg-find-file
zle -N goto-projects-and-find

bindkey '^ ' goto-projects
bindkey '^f' rg-find-file
bindkey '^l' goto-projects-and-find

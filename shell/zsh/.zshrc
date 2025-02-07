# ZSH Home
export ZSH="$HOME/.zsh"
export ZSH_PLUGINS="$ZSH/plugins"

###################################
####### SETTINGS
###################################
export HISTFILE=$ZSH/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

# History won't save duplicates.
setopt HIST_IGNORE_ALL_DUPS

# History won't show duplicates on search.
setopt HIST_FIND_NO_DUPS

CASE_SENSITIVE="true"

export TERM="xterm-256color" 

# vim/nvim option
export EDITOR=vim # change to nvim if using neovim
export VISUAL=vim

# emacs option
# export EDITOR="emacsclient -t -a ''"
# export VISUAL="emacsclient -c -a emacs"

###################################
####### Plugins
###################################

source $ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# source $ZSH_PLUGINS/zsh-vi-mode/zsh-vi-mode.zsh
# _fix_cursor() {
#    echo -ne '\e[5 q'
# }
# precmd_functions+=(_fix_cursor)

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

###################################
####### FZF
###################################

CUSTOM_PROJECTS_DIR_PATH="$HOME/projects/" # Change this based on your projects directory

# Ripgrep
# export FZF_DEFAULT_COMMAND='rg --files --hidden --follow'
# alias fzfi='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}" | fzf'
# alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(find . -type d -print | fzf)' 

# Fd
export FZF_DEFAULT_COMMAND='fd --type f --hidden'
alias fzfi='fd --type file --hidden --no-ignore --exclude .git | fzf'
alias zfd='cd $CUSTOM_PROJECTS_DIR_PATH && cd $(fd . -t d | fzf)'

alias vz='v $(fzfi)'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

###################################
####### ZSH Plugin config
###################################

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#6272a4'
ZSH_HIGHLIGHT_STYLES[path]='fg=#8be9fd'

bindkey '^ ' autosuggest-accept
bindkey -s '^f' "vz^M"
bindkey -s "^l" "zfd^M"
bindkey -s '^p' "zfd && vz^M"

###################################
####### Git
###################################

# Git autocompletetion
zstyle ':completion:*:*:git:*' script ~/.zsh/git-completion.bash
fpath=(~/.zsh $fpath)
autoload -Uz compinit && compinit

autoload -Uz vcs_info # enable vcs_info
precmd () { vcs_info } # always load before displaying the prompt
zstyle ':vcs_info:*' formats '(%b)'
# zstyle ':vcs_info:*' formats ' %b'


parse_git_status() {
  git_status="$(git status 2> /dev/null)"
  [[ "$git_status" =~ "Changes to be committed:" ]] && echo -n "%F{green}·%f "
  [[ "$git_status" =~ "Changes not staged for commit:" ]] && echo -n "%F{yellow}·%f "
  [[ "$git_status" =~ "Untracked files:" ]] && echo -n "%F{red}·%f "
  [[ "$git_status" =~ "Your branch is behind" ]] && echo -n "%F{red}^%f "
  [[ "$git_status" =~ "Your branch is ahead" ]] && echo -n "%F{green}^%f "
  [[ "$git_status" =~ "have diverged" ]] && echo -n "%F{red}!%f "
}

###################################
####### PS1
###################################

setopt PROMPT_SUBST
NEWLINE=$'\n'

PROMPT='%F{81}%n@%m:%f%F{green}${PWD/#$HOME/~}%f %F{183}${vcs_info_msg_0_}%f $(parse_git_status)%F{yellow}$NEWLINE$%f '

### NERD FONT VARIANT
#PROMPT='%F{81}󰌢 %n@%m%f%F{green}  ${PWD/#$HOME/~}%f %F{183}${vcs_info_msg_0_}%f $(parse_git_status)%F{yellow}$NEWLINE%f '

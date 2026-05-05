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

export EDITOR=nvim
export VISUAL=nvim

# export EDITOR="emacsclient -t -a ''"
# export VISUAL="emacsclient -c -a emacs"


# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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
[[ -o interactive ]] || return

if [[ "$TERM" != "xterm-ghostty" ]]; then
  export TERM="xterm-256color"
fi

###################################
####### Plugins
###################################

source $ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#6272a4'
ZSH_HIGHLIGHT_STYLES[path]='fg=#8be9fd'

###################################
####### Bindings
###################################

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

autoload -Uz vcs_info
precmd_functions+=(vcs_info)
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '(%b)'
# zstyle ':vcs_info:git:*' formats ' %b'

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

# No (Nerdfont) Icons Prompt
PROMPT='%F{81}%n:%f%F{green}${PWD/#$HOME/~}%f %F{183}${vcs_info_msg_0_}%f $(parse_git_status)%F{yellow}$NEWLINE$%f '

# (Nerdfont) Icons variant
# PROMPT='%F{81}󰌢 %n%f%F{green}  ${PWD/#$HOME/~}%f %F{183}${vcs_info_msg_0_}%f $(parse_git_status)%F{yellow}${NEWLINE}󰘧%f '

# Lambda variant
# PROMPT='%F{250}󰌢 %f%F{68}${PWD/#$HOME/~}%f %F{146}${vcs_info_msg_0_}%f $(parse_git_status)%F{172}${NEWLINE}󰘧%f '

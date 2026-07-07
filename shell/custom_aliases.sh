###################################
####### Aliases
###################################

if command -v eza >/dev/null 2>&1; then
  alias ls='eza'
  alias ll='eza -l --icons'
  alias tree='eza --tree --icons=always'
else
  alias ls='ls -GFh --color=auto'
  alias ll='ls -l'
fi

alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -pv'
alias less='less -FSRXc'

alias v='nvim'
alias vc='v --clean'
alias ec='emacsclient -n -c -a ""'
alias zj='zellij'
alias path='echo -e ${PATH//:/\\n}'

alias glc='git log -1 --pretty=%B | tr -s "\n"'
alias gbc='git branch --format="%(refname:short)" | grep -v -E "^(main|master)$" | xargs -p git branch -D'

###################################
####### FZF
###################################

CUSTOM_PROJECTS_DIR_PATH="$HOME/projects/"

if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
  export FZF_ALT_C_COMMAND='fd --type d --hidden --exclude .git'
  zfd() {
    local dir
    dir=$(fd . "$CUSTOM_PROJECTS_DIR_PATH" -d 3 -t d | fzf) || return
    [ -n "$dir" ] && cd "$dir"
  }
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g "!{.git}"'
  zfd() {
    local dir
    dir=$(find "$CUSTOM_PROJECTS_DIR_PATH" -maxdepth 3 -name ".git" -prune -o -type d -print | fzf) || return
    [ -n "$dir" ] && cd "$dir"
  }
fi

[ -n "$FZF_DEFAULT_COMMAND" ] && export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

if command -v bat >/dev/null 2>&1; then
  alias cat="bat --paging=never --style=plain"
  export FZF_CTRL_T_OPTS="
  --height=80%
  --layout=reverse
  --border
  --preview-window 'right:60%,50%,wrap'
  --preview 'bat --color=always --style=numbers --line-range=:500 {}'
  "
  vz() {
    local file
    file=$(fzf --preview "bat --color=always --style=numbers --line-range=:500 {}") || return
    [ -n "$file" ] && v "$file"
  }
else
  vz() {
    local file
    file=$(fzf) || return
    [ -n "$file" ] && v "$file"
  }
fi

# tmux
if command -v tmux >/dev/null 2>&1; then
  alias tdev="~/.config/tmux/sessions/dev.sh"
fi

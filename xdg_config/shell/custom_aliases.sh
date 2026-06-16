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

if command -v bat >/dev/null 2>&1; then
  export BAT_OPTS="--paging=never"
  cat() {
    if [ -t 1 ]; then
      bat "$@"
    else
      command cat "$@"
    fi
  }
fi

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

# Ripgrep
# export FZF_DEFAULT_COMMAND='rg --files --hidden -g "!{.git}"'
# alias zfd='cd "$CUSTOM_PROJECTS_DIR_PATH" && cd "$(find . -maxdepth 2 -name ".git" -prune -o -type d -print | fzf)"'


# Fd
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
alias zfd='cd "$CUSTOM_PROJECTS_DIR_PATH" && cd "$(fd . -d 2 -t d | fzf)"'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias vz='v "$(fzf)"'


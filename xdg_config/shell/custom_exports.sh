if command -v nvim >/dev/null 2>&1; then
  export EDITOR="nvim"
  export VISUAL="nvim"

  if [[ -t 1 ]]; then
    export MANPAGER='nvim +Man!'
  fi
else
  export EDITOR="vim"
  export VISUAL="vim"
fi

# emacs option
# export EDITOR="emacsclient -t -a ''"
# export VISUAL="emacsclient -c -a emacs"


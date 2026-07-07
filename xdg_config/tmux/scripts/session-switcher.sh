#!/bin/bash

# Switch/attach to a tmxu session via fzf/ Works inside or outside tmux
TMUX_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null)

if [ -z "$sessions" ]; then
  script=$(find "$TMUX_DIR/sessions" -type f | fzf)
  [ -z "$script" ] && exit 0
  exec "$script"
fi

session=$(echo "$sessions" | fzf)
[ -z "$session" ] && exit 0

if [ -n "$TMUX" ]; then
  current=$(tmux display-message -p '#{session_name}')
  [ "$session" = "$current" ] && exit 0
  tmux switch-client -t "$session"
else
  tmux attach -t "$session"
fi

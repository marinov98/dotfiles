#!/bin/bash

PROJECT=$(fd . ~/projects/ -d 2 -t d | fzf)
[[ -z "$PROJECT" ]] && exit 0

SESSION="$(basename $PROJECT | tr ".:" "__")"
AGENT="pi"

if [ -n "$TMUX" ]; then
  CURRENT_SESSION=$(tmux display-message -p '#S')
  if [ "$CURRENT_SESSION" = "$SESSION" ]; then
    exit 0
  fi
fi

if tmux has-session -t "$SESSION" 2>/dev/null; then
  if [ ! "$TMUX" ]; then
    tmux attach -t "$SESSION"
  else
    tmux switch-client -t "$SESSION"
  fi
  exit 0
fi

# tmux session doesn't exist, create new
tmux new-session -d -s "$SESSION" -c "$PROJECT" -n "editor"
tmux send-keys -t "$SESSION:editor" "$EDITOR" C-m

if command -v lazygit >/dev/null 2>&1 \
    && git -C "$PROJECT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  tmux new-window -t "$SESSION" -c "$PROJECT" -n "lazygit"
  tmux send-keys -t "$SESSION:lazygit" "lazygit" C-m
fi

if command -v "$AGENT" >/dev/null 2>&1; then
  tmux new-window -t "$SESSION" -c "$PROJECT" -n "$AGENT"
  tmux send-keys -t "$SESSION:$AGENT" "$AGENT" C-m
fi

tmux new-window -t "$SESSION" -c "$PROJECT"

tmux select-window -t "$SESSION:editor"

if [ ! "$TMUX" ]; then
  tmux attach -t $SESSION
else
  tmux switch-client -t "$SESSION"
fi

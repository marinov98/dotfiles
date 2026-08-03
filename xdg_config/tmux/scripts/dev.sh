#!/bin/bash

TARGET_DEV_DIR="$HOME/projects"

PROJECT=$(find $TARGET_DEV_DIR/ -mindepth 1 -maxdepth 2 -name '.*' -prune -o -type d -print  | fzf)

[[ -z "$PROJECT" ]] && exit 0

SESSION="$(basename "$PROJECT" | tr ".:" "__")"

# Short-circuit if we are already in the target session
if [ -n "$TMUX" ] && [ "$(tmux display-message -p '#S')" = "$SESSION" ]; then
  exit 0
fi

AGENT="pi"
AGENT_CMD="pi"

connect_to_session() {
  if [ -z "$TMUX" ]; then
    tmux attach -t "$1"
  else
    tmux switch-client -t "$1"
  fi
  exit 0
}

if tmux has-session -t "$SESSION" 2>/dev/null; then
  connect_to_session "$SESSION"
fi

# Session doesn't exist, build it out
tmux new-session -d -s "$SESSION" -c "$PROJECT" -n "dev"

if command -v "$AGENT" >/dev/null 2>&1; then
  tmux new-window -t "$SESSION" -c "$PROJECT" -n "$AGENT"
  tmux send-keys -t "$SESSION:$AGENT" "$AGENT_CMD" C-m
fi

tmux select-window -t "$SESSION:dev"
connect_to_session "$SESSION"

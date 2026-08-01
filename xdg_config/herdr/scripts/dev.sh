#!/bin/bash

TARGET_DEV_DIR="$HOME/projects"
PROJECT=$(find $TARGET_DEV_DIR/ -mindepth 1 -maxdepth 2 -type d | fzf)

[[ -z "$PROJECT" ]] && exit 0

TARGET_LABEL="$(basename "$PROJECT")"
WORKSPACES=$(herdr workspace list | jq '.result.workspaces')
IN_HERDR=$( [ "${HERDR_ENV:-}" = "1" ] && [ -n "${HERDR_WORKSPACE_ID}" ] )


if $IN_HERDR; then
  FOCUSED_LABEL=$(echo "$WORKSPACES" | jq -r '.[] | select(.focused == true) | .label')
  # Already focused on target
  [ "$FOCUSED_LABEL" = "$TARGET_LABEL" ] && exit 0
fi

# Workspace exists, focus it
TARGET_ID=$(echo "$WORKSPACES" | jq -r --arg label "$TARGET_LABEL" '.[] | select(.label == $label) | .workspace_id')
if [ -n "$TARGET_ID" ]; then
  $IN_HERDR && herdr workspace focus "$TARGET_ID" || herdr
  exit 0
fi

# Create and focus
WORKSPACE_ID=$(herdr workspace create --cwd "$PROJECT" --label "$TARGET_LABEL" --focus | jq -r '.result.workspace.workspace_id')


AGENT="pi"
AGENT_CMD="pi"

if command -v "$AGENT" >/dev/null 2>&1; then
  AGENT_PANE_ID=$(herdr tab create --workspace "$WORKSPACE_ID" --cwd "$PROJECT" --label "$AGENT" | jq -r '.result.root_pane.pane_id')
  herdr pane run "$AGENT_PANE_ID" $AGENT_CMD
fi

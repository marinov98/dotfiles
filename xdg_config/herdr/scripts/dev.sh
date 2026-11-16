#!/bin/bash

# only execute within herdr
if [ "${HERDR_ENV:-}" != "1" ] || [ -z "${HERDR_WORKSPACE_ID}" ]; then
  echo "Script can only be run inside herdr!"
  exit 1
fi

TARGET_DEV_DIR="$HOME/projects"
PROJECT=$(find $TARGET_DEV_DIR/ -mindepth 1 -maxdepth 2 -name '.*' -prune -o -type d -print  | fzf)

[[ -z "$PROJECT" ]] && exit 0

TARGET_LABEL="$(basename "$PROJECT")"
WORKSPACES=$(herdr workspace list | jq '.result.workspaces')


FOCUSED_LABEL=$(echo "$WORKSPACES" | jq -r '.[] | select(.focused == true) | .label')
# Already focused on target
[ "$FOCUSED_LABEL" = "$TARGET_LABEL" ] && exit 0

# Workspace exists, focus it
TARGET_ID=$(echo "$WORKSPACES" | jq -r --arg label "$TARGET_LABEL" '.[] | select(.label == $label) | .workspace_id')
if [ -n "$TARGET_ID" ]; then
  herdr workspace focus "$TARGET_ID"
  exit 0
fi

if [ "$FOCUSED_LABEL" == "~" ]; then
  # turn default workspace into a project
  HOME_PANE=$(herdr pane current | jq '.result.pane') 
  HOME_PANE_ID=$(echo $HOME_PANE | jq -r '.pane_id')
  WORKSPACE_ID=$(echo "$HOME_PANE" | jq -r '.workspace_id')

  herdr pane run "$HOME_PANE_ID" "cd '$PROJECT'"
else
  # Create and focus new project
  WORKSPACE_ID=$(herdr workspace create --cwd "$PROJECT" --label "$TARGET_LABEL" --focus | jq -r '.result.workspace.workspace_id')
fi


AGENT="pi"
AGENT_CMD="pi"

if command -v "$AGENT" >/dev/null 2>&1; then
  AGENT_PANE_ID=$(herdr tab create --workspace "$WORKSPACE_ID" --cwd "$PROJECT" --label "$AGENT" | jq -r '.result.root_pane.pane_id')
  herdr pane run "$AGENT_PANE_ID" $AGENT_CMD
fi

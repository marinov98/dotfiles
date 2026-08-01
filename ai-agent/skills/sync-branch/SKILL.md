---
name: sync-branch
description: Use this skill when the user wants to synchronize their current branch with another branch. Prioritizes linear history using fast-forward merges and rebasing.
---

# Sync Branch Skill

## Principles

1. **Prefer fast-forward merges** - Never create merge commits when syncing branches
2. **Prefer rebasing** - Use `git rebase` over `git merge` for incorporating changes
3. **Never force push** to shared branches unless explicitly requested
4. **Preserve commit history** - Avoid squashing or amending commits that have been pushed

## CRITICAL: Never Block on Interactive Prompts

Git opens editors/prompts (commit messages, rebase todos, merge messages) and will
**HANG forever in non-TTY shells** — this has repeatedly blocked sessions.
Always run state-changing git commands non-interactively:

- `GIT_EDITOR=true git rebase --continue` — finalize a conflicted commit without opening an editor
- `GIT_EDITOR=true GIT_SEQUENCE_EDITOR=true git rebase -i ...` — scripted interactive rebases
- `git commit --no-edit` — reuse the prepared message without an editor
- `git merge --no-edit` / `git pull --no-edit` — skip the merge-message editor
- `git rebase --continue` with a TODO editor: same `GIT_EDITOR=true GIT_SEQUENCE_EDITOR=true` prefix

If a command returns no output and appears stuck, it is almost always waiting on an
editor or prompt. Check `ps aux | grep -iE 'git|editor|vim|nano'`. The rebase state
in `.git/rebase-merge` is safe to resume: re-run with `GIT_EDITOR=true git rebase --continue`
instead of killing or aborting. Inspect progress with `git status` (it reports rebase
done/todo counts) and `cat .git/rebase-merge/done`.

Also prefer explicit flags over prompts: `git push --force-with-lease` never needs
confirmation, `git stash` is never interactive.

## Workflow

### Step 1: Analyze Current State

```bash
git status
git branch -vv
git log --oneline -5
```

### Step 2: Identify Target Branch

- Determine the base branch to sync with (main, master, develop, etc.)
- Check if the current branch has unpushed commits

### Step 3: Sync Strategy

**If current branch has NO unpushed commits:**

```
git fetch origin
git rebase origin/<target-branch>
```

**If current branch HAS unpushed commits:**

1. If commits are ready to share: commit them first, then rebase
2. If commits are work-in-progress:
   - Option A: Stash changes (`git stash`), rebase, then `git stash pop`
   - Option B: Create a backup branch (`git branch backup`), rebase, then cherry-pick or rebase the backup onto the new base
   - Option C: Rebase with `--keep-empty` flag

### Step 4: Resolve Conflicts (if any)

```
git status  # identify conflicted files
# Edit conflicted files to resolve (follow the user's stated preference, e.g. "keep branch X's changes")
git add <resolved-files>
GIT_EDITOR=true git rebase --continue   # NEVER plain `git rebase --continue` — it opens an editor and hangs in non-TTY shells
```

Repeat until `git rebase` reports success.

### Step 5: Verify Success

```
git log --oneline --graph -10
git status
```

## Abort Rebase

If something goes wrong:

```
git rebase --abort
```

## Force Push (Only if necessary)

If rebasing was done on a private feature branch:

```
git push --force-with-lease
```

Do NOT use `--force` as it is unsafe.

## Branch Protection

Never force push to:

- main/master
- protected branches
- Shared/public branches

Always confirm with the user before any destructive operation.

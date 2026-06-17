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
# Edit conflicted files to resolve
git add <resolved-files>
git rebase --continue
```

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

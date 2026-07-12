---
name: commit-message
description: Use this skill when the user wants to commit code changes. Follows the Conventional Commits specification.
---

# Commit Message Skill

## Conventional Commit Format

```
<type>(<scope>): <subject>

[optional body]

[optional footer(s)]
```

## Commit Types

- **feat**: A new feature
- **fix**: A bug fix
- **docs**: Documentation only changes
- **style**: Changes that do not affect the meaning of the code (formatting, etc.)
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **perf**: A code change that improves performance
- **test**: Adding missing tests or correcting existing tests
- **build**: Changes that affect the build system or dependencies
- **ci**: Changes to CI configuration files and scripts
- **chore**: Other changes that don't modify src or test files
- **revert**: Reverts a previous commit

## Scope

The scope is optional and should describe the part of the codebase affected (e.g., `feat(api)`, `fix(auth)`, `docs(readme)`).

## Rules

1. Use imperative mood in the subject line (e.g., "add feature" not "added feature")
2. Do not end the subject line with a period
3. Keep the subject line under 72 characters
4. Separate subject from body with a blank line
5. Use the body to explain _what_ and _why_ (not _how_)
6. Include issue/PR references in the footer when applicable

## Test Grouping

When committing changes that include both feature/refactoring work and corresponding tests, always commit them together in a single commit. This prevents CI breakage where features are updated but tests are not, causing test failures.

**Example of problematic separate commits:**
- `feat: add new validation logic` (feature without tests)
- `test: add tests for validation` (tests added later)

**Correct approach - single commit:**
- `feat: add new validation logic with tests`

Only separate test commits when:
- Adding tests for existing code without modifying the code itself
- Fixing broken tests without changing the feature code
- Updating test infrastructure or configuration

## Workflow

1. Run `git status` to see all changes
2. Run `git diff` to review the changes
3. Analyze the changes to determine the appropriate type and scope
4. Check if changes include both feature/refactoring and tests - if so, commit them together
5. Write a concise, descriptive commit message following the format above
6. Ask the user to confirm before committing, or commit if they approve
7. Run `git log -1` to verify the commit was created correctly

## Amending Commits

If a commit is missing changes that should have been part of it, amend the commit rather than creating a new "fix" commit. One task = one commit.

- **Before pushing**: Always safe to amend
- **After pushing**: Amending is acceptable on feature branches before merge to maintain clean history. Use `git commit --amend` and force push if needed.

**Do not** create separate commits like "fix: forgot to add X" or "chore: address review feedback" for changes that belong in the original commit.

## Pushing

When pushing to remote, use `git push origin <branch>` instead of `--set-upstream` to avoid changing the upstream configuration.

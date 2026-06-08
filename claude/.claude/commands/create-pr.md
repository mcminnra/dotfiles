---
allowed-tools: Bash(git log:*), Bash(git diff:*), Bash(gh pr create:*)
description: Create a PR from the current branch to main
---

## Context

- Current branch: !`git branch --show-current`
- Commits since main: !`git log --oneline main..HEAD`
- Diff from main: !`git diff main...HEAD --stat`

## Your task

Create a pull request from the current branch to `main`.

**PR title format:** `type(scope): description`
- Types: `feat`, `fix`, `refactor`, `docs`, `chore`, `style`, `test`
- Infer the type, scope, and description from the commits and diff

**PR body format:** A flat bullet list — one sentence per bullet, no headers, no sections.
Each bullet starts with a conventional prefix: `feat:`, `fix:`, `refactor:`, `docs:`, `chore:`, `style:`, or `test:`.
Derive the bullets from the commits and diff — one bullet per logical change.

**Hard rules:**
- No AI attribution of any kind
- Use `gh pr create` with `--base main`

Create the PR and output the URL. No other output.

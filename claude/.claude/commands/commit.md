---
allowed-tools: Bash(git diff:*), Bash(git status:*), Bash(git commit:*)
description: Stage all changes and commit with a conventional commit message derived from the diff
---

## Context

- Status: !`git status --short`
- Staged diff: !`git diff --cached --stat`
- Unstaged diff: !`git diff --stat`
- Full diff: !`git diff HEAD`

## Your task

Write a single-line conventional commit message for the changes above and run `git commit`.

**Message format:** `type: short summary`
- Types: `feat`, `fix`, `refactor`, `docs`, `chore`
- Summary: imperative mood, lowercase, ≤60 chars, no period

**Hard rules:**
- Single line only — no body, no bullet points
- No `Co-Authored-By` or any AI attribution
- If nothing is staged, stage all changes first with `git add -A` before committing
- Output only the commit message you used. Nothing else.

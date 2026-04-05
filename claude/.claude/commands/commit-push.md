---
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git diff:*), Bash(git commit:*), Bash(git push:*)
description: Create a git commit following conventional commits format
---

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Recent commits: !`git log --oneline -10`

## Your task

Stage all changed files and create a single commit with these rules:

**Format:** `type(scope): description`
- Types: `feat`, `fix`, `refactor`, `docs`, `chore`, `style`, `test`
- Scope: the logical component being changed — infer from the diff and recent commits
- Description: lowercase, imperative, no period at end — use the diff to write a concise, accurate message

**Hard rules:**
- Single-line message only — no body, no footer
- No "Co-Authored-By" line
- No AI attribution of any kind

Stage, commit, and push in a single flow. No other output.

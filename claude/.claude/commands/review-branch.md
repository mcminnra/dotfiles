---
allowed-tools: Bash(git log:*), Bash(git diff:*), Read
description: Review commits on current branch since main for potential issues
---

## Context

- Current branch: !`git branch --show-current`
- Commits since main: !`git log --oneline main..HEAD`
- Full diff from main: !`git diff main...HEAD`

## Your task

Review all changes on this branch since `main`. Analyze the diff for:

1. **Bugs** — logic errors, off-by-ones, null/undefined risks, race conditions
2. **Security** — injection, secrets, unsafe inputs, missing auth
3. **Config issues** — typos, invalid values, missing required fields
4. **Breaking changes** — removed exports, changed interfaces, renamed keys

Read any files needed for additional context.

**Output a markdown table:**

| Severity | Category | File | Line(s) | Issue |
|----------|----------|------|---------|-------|

Severity levels: 🔴 High, 🟡 Medium, 🔵 Low

If no issues found, say "No issues found." No other output.

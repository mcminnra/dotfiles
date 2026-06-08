#!/usr/bin/env bash
set -uo pipefail

INPUT=$(cat)

SESSION_ID=$(echo "$INPUT" | grep -o '"session_id":"[^"]*"' | head -1 | sed 's/"session_id":"//;s/"$//')
CWD=$(echo "$INPUT" | grep -o '"cwd":"[^"]*"' | head -1 | sed 's/"cwd":"//;s/"$//')

if [[ -z "$SESSION_ID" ]] || [[ -z "$CWD" ]]; then
    exit 0
fi

# Check if cwd is inside a git repo
if ! git -C "$CWD" rev-parse --git-dir &>/dev/null 2>&1; then
    exit 0
fi

LATEST_HASH=$(git -C "$CWD" log -1 --format="%H" 2>/dev/null)
if [[ -z "$LATEST_HASH" ]]; then
    exit 0
fi

STATE_DIR="${TMPDIR:-/tmp}"
STATE_FILE="$STATE_DIR/lo_${SESSION_ID//[^a-zA-Z0-9_-]/_}.state"

LAST_HASH=""
OFFER_COUNT=0
if [[ -f "$STATE_FILE" ]]; then
    LAST_HASH=$(sed -n '1p' "$STATE_FILE" 2>/dev/null || echo "")
    OFFER_COUNT=$(sed -n '2p' "$STATE_FILE" 2>/dev/null || echo 0)
fi

# No new commit since last check
if [[ "$LATEST_HASH" == "$LAST_HASH" ]]; then
    exit 0
fi

# Update stored hash regardless of whether we offer
printf '%s\n%s\n' "$LATEST_HASH" "$OFFER_COUNT" > "$STATE_FILE"

# Already offered twice this session
if [[ "$OFFER_COUNT" -ge 2 ]]; then
    exit 0
fi

# New commit detected — increment offer count and emit nudge
printf '%s\n%s\n' "$LATEST_HASH" $(( OFFER_COUNT + 1 )) > "$STATE_FILE"

cat <<'HOOK_JSON'
{"continue":true,"systemMessage":"[learning-opportunities] A new git commit was just made. Consider whether this is a good moment to offer a learning exercise. If the committed work involved new files, schema changes, architectural decisions, refactors, or unfamiliar patterns, ask the user (one short sentence) if they'd like a 10-15 minute exercise using /learning-opportunities. Do not start the exercise until they confirm. If they decline, no more offers this session."}
HOOK_JSON

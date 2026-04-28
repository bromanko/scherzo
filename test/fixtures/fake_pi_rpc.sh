#!/usr/bin/env bash
# Test fixture for Scherzo pi RPC tests. This is not a production pi substitute.
set -euo pipefail

if [[ -n "${FAKE_PI_STDERR:-}" ]]; then
  echo "fake pi diagnostic: ${FAKE_PI_STDERR}" >&2
fi

if [[ -n "${FAKE_PI_NEVER_END:-}" ]]; then
  while true; do sleep 60; done
fi

prompt_seen=0

while IFS= read -r line; do
  if [[ -n "${FAKE_PI_TRANSCRIPT:-}" ]]; then
    printf '%s\n' "$line" >> "$FAKE_PI_TRANSCRIPT"
  fi

  if [[ -n "${FAKE_PI_DELAY_MS:-}" ]]; then
    sleep "$(awk "BEGIN { print ${FAKE_PI_DELAY_MS} / 1000 }")"
  fi

  if [[ -n "${FAKE_PI_MALFORMED:-}" ]]; then
    echo '{this is not json'
    exit 0
  fi

  id="$(printf '%s' "$line" | jq -r '.id // ""')"
  type="$(printf '%s' "$line" | jq -r '.type // ""')"

  case "$type" in
    set_session_name)
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"set_session_name",success:true}'
      ;;
    set_auto_retry)
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"set_auto_retry",success:true}'
      ;;
    get_state)
      jq -cn --arg id "$id" --arg cwd "$PWD" '{id:$id,type:"response",command:"get_state",success:true,data:{sessionId:"fake-session",sessionFile:null,isStreaming:false,cwd:$cwd}}'
      ;;
    get_session_stats)
      if [[ -n "${FAKE_PI_STATS_FAIL:-}" ]]; then
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"get_session_stats",success:false,error:"stats failed"}'
      else
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"get_session_stats",success:true,data:{tokens:{input:1,output:2,cacheRead:0,cacheWrite:0,total:3}}}'
      fi
      ;;
    abort)
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"abort",success:true}'
      ;;
    extension_ui_response)
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"extension_ui_response",success:true}'
      ;;
    prompt)
      prompt_seen=$((prompt_seen + 1))
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"prompt",success:true}'
      if [[ -n "${FAKE_PI_NO_OUTPUT_AFTER_PROMPT:-}" ]]; then
        while true; do sleep 60; done
      fi
      if [[ -n "${FAKE_PI_DELAY_EVENT_MS:-}" ]]; then
        sleep "$(awk "BEGIN { print ${FAKE_PI_DELAY_EVENT_MS} / 1000 }")"
      fi
      jq -cn '{type:"agent_start"}'
      jq -cn '{type:"turn_start"}'
      if [[ -n "${FAKE_PI_MESSAGE_SECRET:-}" ]]; then
        jq -cn --arg secret "$FAKE_PI_MESSAGE_SECRET" '{type:"message_update",delta:("POPULATED " + $secret),authorization:$secret,nested:{token:$secret}}'
      elif [[ -f POPULATED ]]; then
        jq -cn '{type:"message_update",delta:"POPULATED"}'
      else
        jq -cn '{type:"message_update",delta:"not-populated"}'
      fi
      if [[ -n "${FAKE_PI_UI_DIALOG:-}" ]]; then
        jq -cn '{id:"ui-1",type:"extension_ui_request",method:"confirm",message:"continue?"}'
      fi
      if [[ -n "${FAKE_PI_UI_NOTIFY:-}" ]]; then
        jq -cn '{type:"extension_ui_request",method:"notify",message:"hello"}'
      fi
      if [[ -n "${FAKE_PI_STALL_AFTER_PROMPT:-}" ]]; then
        sleep "$(awk "BEGIN { print ${FAKE_PI_STALL_AFTER_PROMPT} / 1000 }")"
      fi
      jq -cn '{type:"turn_end"}'
      if [[ -n "${FAKE_PI_NO_AGENT_END:-}" ]]; then
        while true; do sleep 60; done
      fi
      jq -cn --arg turns "$prompt_seen" '{type:"agent_end",messages:[{role:"assistant",content:"done"}],turns:($turns|tonumber)}'
      if [[ -n "${FAKE_PI_EXIT_NONZERO:-}" ]]; then
        exit 7
      fi
      ;;
    *)
      jq -cn --arg id "$id" --arg command "$type" '{id:$id,type:"response",command:$command,success:false,error:"unknown command"}'
      ;;
  esac
done

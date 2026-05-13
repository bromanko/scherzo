#!/usr/bin/env bash
# Test fixture for Scherzo pi RPC tests. This is not a production pi substitute.
set -euo pipefail

if [[ -n "${FAKE_PI_STDERR:-}" ]]; then
  echo "fake pi diagnostic: ${FAKE_PI_STDERR}" >&2
fi

if [[ -n "${FAKE_PI_PID_FILE:-}" ]]; then
  printf '%s\n' "$$" > "$FAKE_PI_PID_FILE"
fi

if [[ -n "${FAKE_PI_NEVER_END:-}" ]]; then
  while true; do sleep 60; done
fi

prompt_seen=0
launched_session_file=""

for ((i = 1; i <= $#; i++)); do
  arg="${!i}"
  if [[ "$arg" == "--session" ]]; then
    next_index=$((i + 1))
    if [[ "$next_index" -le "$#" ]]; then
      launched_session_file="${!next_index}"
    fi
  fi
done

if [[ -n "${FAKE_PI_ARGV_LOG:-}" ]]; then
  {
    printf 'cwd=%s\n' "$PWD"
    printf 'argv[0]=%s\n' "$0"
    index=1
    for arg in "$@"; do
      printf 'argv[%s]=%s\n' "$index" "$arg"
      index=$((index + 1))
    done
  } >> "$FAKE_PI_ARGV_LOG"
fi

if [[ -n "${FAKE_PI_ENV_LOG:-}" ]]; then
  {
    printf 'SCHERZO_WORKSPACE_PROFILE=%s\n' "${SCHERZO_WORKSPACE_PROFILE:-}"
    printf 'SCHERZO_WORKSPACE_DRIVER=%s\n' "${SCHERZO_WORKSPACE_DRIVER:-}"
    printf 'SCHERZO_WORKSPACE_CAPABILITIES=%s\n' "${SCHERZO_WORKSPACE_CAPABILITIES:-}"
    printf 'SCHERZO_JJ_WORKSPACE_BASE=%s\n' "${SCHERZO_JJ_WORKSPACE_BASE:-}"
    printf 'PATH=%s\n' "${PATH:-}"
    printf 'SCHERZO_WORKSPACE_NAME=%s\n' "${SCHERZO_WORKSPACE_NAME:-}"
    printf 'SCHERZO_WORKSPACE_PATH=%s\n' "${SCHERZO_WORKSPACE_PATH:-}"
  } >> "$FAKE_PI_ENV_LOG"
fi

session_file_for_state() {
  if [[ -n "${FAKE_PI_SESSION_FILE_MISMATCH:-}" ]]; then
    printf '%s' "$FAKE_PI_SESSION_FILE_MISMATCH"
  elif [[ -n "${FAKE_PI_SESSION_FILE:-}" ]]; then
    printf '%s' "$FAKE_PI_SESSION_FILE"
  elif [[ -n "$launched_session_file" ]]; then
    printf '%s' "$launched_session_file"
  fi
}

cwd_for_state() {
  if [[ -n "${FAKE_PI_CWD_MISMATCH:-}" ]]; then
    printf '%s' "$FAKE_PI_CWD_MISMATCH"
  else
    printf '%s' "$PWD"
  fi
}

record_input() {
  local input_line="$1"
  if [[ -n "${FAKE_PI_TRANSCRIPT:-}" ]]; then
    printf '%s\n' "$input_line" >> "$FAKE_PI_TRANSCRIPT"
  fi
}

maybe_block_after_message_update() {
  if [[ -n "${FAKE_PI_AFTER_MESSAGE_UPDATE_MARKER:-}" ]]; then
    mkdir -p "$(dirname "$FAKE_PI_AFTER_MESSAGE_UPDATE_MARKER")"
    : > "$FAKE_PI_AFTER_MESSAGE_UPDATE_MARKER"
  fi

  if [[ -n "${FAKE_PI_AFTER_MESSAGE_UPDATE_RELEASE:-}" ]]; then
    local waited_ms=0
    local timeout_ms="${FAKE_PI_AFTER_MESSAGE_UPDATE_TIMEOUT_MS:-5000}"
    while [[ ! -e "$FAKE_PI_AFTER_MESSAGE_UPDATE_RELEASE" ]]; do
      if [[ "$waited_ms" -ge "$timeout_ms" ]]; then
        echo "timed out waiting for FAKE_PI_AFTER_MESSAGE_UPDATE_RELEASE=$FAKE_PI_AFTER_MESSAGE_UPDATE_RELEASE" >&2
        exit 124
      fi
      sleep 0.01
      waited_ms=$((waited_ms + 10))
    done
  fi
}

maybe_interleave_event() {
  if [[ -n "${FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE:-}" ]]; then
    jq -cn '{type:"message_update",delta:"interleaved"}'
  fi
}

handle_nested_command_line() {
  local nested_line="$1"
  record_input "$nested_line"
  local nested_id nested_type
  nested_id="$(printf '%s' "$nested_line" | jq -r '.id // ""')"
  nested_type="$(printf '%s' "$nested_line" | jq -r '.type // ""')"
  case "$nested_type" in
    abort)
      maybe_interleave_event
      jq -cn --arg id "$nested_id" '{id:$id,type:"response",command:"abort",success:true}'
      ;;
    extension_ui_response)
      maybe_interleave_event
      jq -cn --arg id "$nested_id" '{id:$id,type:"response",command:"extension_ui_response",success:true}'
      ;;
    *)
      jq -cn --arg id "$nested_id" --arg command "$nested_type" '{id:$id,type:"response",command:$command,success:false,error:"unexpected nested command"}'
      ;;
  esac
}

abortable_stall() {
  local remaining_ms="${FAKE_PI_ABORTABLE_STALL_MS:-0}"
  while [[ "$remaining_ms" -gt 0 ]]; do
    if IFS= read -r -t 0.01 nested_line; then
      handle_nested_command_line "$nested_line"
    fi
    sleep 0.01
    remaining_ms=$((remaining_ms - 20))
  done
}

while IFS= read -r line; do
  record_input "$line"

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
    set_auto_compaction)
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"set_auto_compaction",success:true}'
      ;;
    compact)
      if [[ -n "${FAKE_PI_COMPACT_FAIL:-}" ]]; then
        if [[ -n "${FAKE_PI_COMPACT_EVENTS_BEFORE_FAIL:-}" ]]; then
          jq -cn '{type:"compaction_start",reason:"manual"}'
          jq -cn '{type:"compaction_end",reason:"manual"}'
        fi
        if [[ -n "${FAKE_PI_COMPACT_NO_RESPONSE_AFTER_EVENTS:-}" ]]; then
          while true; do sleep 60; done
        fi
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"compact",success:false,error:"compact failed"}'
      else
        jq -cn '{type:"compaction_start",reason:"manual"}'
        jq -cn '{type:"compaction_end",reason:"manual"}'
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"compact",success:true}'
      fi
      ;;
    get_state)
      if [[ -n "${FAKE_PI_GET_STATE_FAIL:-}" ]]; then
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"get_state",success:false,error:"get_state failed"}'
      else
        session_id="${FAKE_PI_SESSION_ID:-fake-session}"
        session_file="$(session_file_for_state)"
        cwd="$(cwd_for_state)"
        if [[ -n "$session_file" ]]; then
          jq -cn --arg id "$id" --arg session_id "$session_id" --arg session_file "$session_file" --arg cwd "$cwd" '{id:$id,type:"response",command:"get_state",success:true,data:{sessionId:$session_id,sessionFile:$session_file,isStreaming:false,cwd:$cwd}}'
        else
          jq -cn --arg id "$id" --arg session_id "$session_id" --arg cwd "$cwd" '{id:$id,type:"response",command:"get_state",success:true,data:{sessionId:$session_id,sessionFile:null,isStreaming:false,cwd:$cwd}}'
        fi
      fi
      ;;
    get_session_stats)
      if [[ -n "${FAKE_PI_STATS_FAIL:-}" ]]; then
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"get_session_stats",success:false,error:"stats failed"}'
      else
        jq -cn --arg id "$id" '{id:$id,type:"response",command:"get_session_stats",success:true,data:{tokens:{input:1,output:2,cacheRead:0,cacheWrite:0,total:3}}}'
      fi
      ;;
    abort)
      maybe_interleave_event
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"abort",success:true}'
      ;;
    extension_ui_response)
      maybe_interleave_event
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"extension_ui_response",success:true}'
      ;;
    prompt)
      prompt_seen=$((prompt_seen + 1))
      if [[ -n "${FAKE_PI_INTERLEAVE_EVENT_BEFORE_PROMPT_RESPONSE:-}" ]]; then
        jq -cn '{type:"message_update",delta:"interleaved"}'
      fi
      jq -cn --arg id "$id" '{id:$id,type:"response",command:"prompt",success:true}'
      if [[ -n "${FAKE_PI_NO_OUTPUT_AFTER_PROMPT:-}" ]]; then
        while true; do sleep 60; done
      fi
      if [[ -n "${FAKE_PI_CONTEXT_ERROR_ALWAYS:-}" ]] || { [[ -n "${FAKE_PI_CONTEXT_ERROR_ONCE:-}" ]] && [[ "$prompt_seen" -eq 1 ]]; }; then
        jq -cn '{type:"agent_start"}'
        jq -cn '{type:"turn_start"}'
        jq -cn '{type:"turn_end",stopReason:"error",message:{role:"assistant",provider:"openai-codex",stopReason:"error",errorMessage:"Codex error: {\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"code\":\"context_length_exceeded\",\"message\":\"Your input exceeds the context window of this model. Please adjust your input and try again.\",\"param\":\"input\"},\"sequence_number\":2}",content:[]}}'
        continue
      fi
      if [[ -n "${FAKE_PI_DELAY_EVENT_MS:-}" ]]; then
        sleep "$(awk "BEGIN { print ${FAKE_PI_DELAY_EVENT_MS} / 1000 }")"
      fi
      jq -cn '{type:"agent_start"}'
      jq -cn '{type:"turn_start"}'
      if [[ -n "${FAKE_PI_AUTO_RETRY_SUCCESS:-}" ]]; then
        jq -cn '{type:"message_update",delta:"first attempt"}'
        jq -cn '{type:"turn_end",stopReason:"error",errorMessage:"provider_transport_failure: WebSocket error",message:{role:"assistant",content:[{type:"text",text:"first attempt failed"}]}}'
        jq -cn '{type:"auto_retry_start",attempt:1,maxAttempts:3,delayMs:1,errorMessage:"WebSocket error"}'
        jq -cn '{type:"turn_start"}'
        jq -cn '{type:"message_update",delta:"POPULATED"}'
        jq -cn '{type:"turn_end",message:{role:"assistant",content:[{type:"text",text:"POPULATED"}]}}'
        jq -cn --arg turns "$prompt_seen" '{type:"agent_end",messages:[{role:"assistant",content:"done after retry"}],turns:($turns|tonumber)}'
        jq -cn '{type:"auto_retry_end",success:true,attempt:1}'
        continue
      fi
      if [[ -n "${FAKE_PI_AUTO_RETRY_EXHAUSTED:-}" ]]; then
        jq -cn '{type:"message_update",delta:"first attempt"}'
        jq -cn '{type:"turn_end",stopReason:"error",errorMessage:"provider_transport_failure: WebSocket error",message:{role:"assistant",content:[{type:"text",text:"first attempt failed"}]}}'
        jq -cn '{type:"auto_retry_start",attempt:1,maxAttempts:2,delayMs:1,errorMessage:"WebSocket error"}'
        jq -cn '{type:"turn_start"}'
        jq -cn '{type:"turn_end",stopReason:"error",errorMessage:"provider_transport_failure: ECONNRESET",message:{role:"assistant",content:[{type:"text",text:"retry failed"}]}}'
        jq -cn --arg turns "$prompt_seen" '{type:"agent_end",turns:($turns|tonumber)}'
        jq -cn '{type:"auto_retry_end",success:false,attempt:2,finalError:"provider_transport_failure"}'
        continue
      fi
      if [[ -n "${FAKE_PI_RETRYABLE_ERROR_NO_RETRY_EVENT:-}" ]]; then
        jq -cn '{type:"turn_end",stopReason:"error",errorMessage:"provider_transport_failure: WebSocket error",message:{role:"assistant",content:[{type:"text",text:"provider failed"}]}}'
        jq -cn --arg turns "$prompt_seen" '{type:"agent_end",turns:($turns|tonumber)}'
        continue
      fi
      if [[ -n "${FAKE_PI_MESSAGE_SECRET:-}" ]]; then
        fake_pi_text="POPULATED ${FAKE_PI_MESSAGE_SECRET}"
        jq -cn --arg secret "$FAKE_PI_MESSAGE_SECRET" '{type:"message_update",delta:("POPULATED " + $secret),authorization:$secret,nested:{token:$secret}}'
      elif [[ -f POPULATED ]]; then
        fake_pi_text="POPULATED"
        jq -cn '{type:"message_update",delta:"POPULATED"}'
      else
        fake_pi_text="not-populated"
        jq -cn '{type:"message_update",delta:"not-populated"}'
      fi
      maybe_block_after_message_update
      if [[ -n "${FAKE_PI_TOOL:-}" ]]; then
        if [[ -n "${FAKE_PI_TOOL_SECRET:-}" ]]; then
          jq -cn --arg secret "$FAKE_PI_TOOL_SECRET" '{type:"message",message:{role:"assistant",content:[{type:"toolCall",id:"call_fake",name:"bash",arguments:{command:("gleam test " + $secret)}}]}}'
          jq -cn --arg secret "$FAKE_PI_TOOL_SECRET" '{type:"message",message:{role:"toolResult",toolCallId:"call_fake",toolName:"bash",content:[{type:"text",text:("2 failures " + $secret)}],isError:true}}'
        else
          jq -cn '{type:"message",message:{role:"assistant",content:[{type:"toolCall",id:"call_fake",name:"bash",arguments:{command:"gleam test"}}]}}'
          jq -cn '{type:"message",message:{role:"toolResult",toolCallId:"call_fake",toolName:"bash",content:[{type:"text",text:"2 failures"}],isError:true}}'
        fi
      fi
      if [[ -n "${FAKE_PI_UI_DIALOG:-}" ]]; then
        jq -cn '{id:"ui-1",type:"extension_ui_request",method:"confirm",message:"continue?"}'
        if [[ -n "${FAKE_PI_UI_DIALOG_WAITS:-}" ]]; then
          if IFS= read -r ui_line; then
            handle_nested_command_line "$ui_line"
          fi
        fi
      fi
      if [[ -n "${FAKE_PI_UI_NOTIFY:-}" ]]; then
        jq -cn '{type:"extension_ui_request",method:"notify",message:"hello"}'
      fi
      if [[ -n "${FAKE_PI_ABORTABLE_STALL_MS:-}" ]]; then
        abortable_stall
      fi
      if [[ -n "${FAKE_PI_STALL_AFTER_PROMPT:-}" ]]; then
        sleep "$(awk "BEGIN { print ${FAKE_PI_STALL_AFTER_PROMPT} / 1000 }")"
      fi
      if [[ -n "${FAKE_PI_STOP_REASON_ERROR:-}" ]]; then
        jq -cn --arg text "$fake_pi_text" '{type:"turn_end",stopReason:"error",errorMessage:"semantic model error",message:{role:"assistant",content:[{type:"text",text:$text}]}}'
      else
        jq -cn --arg text "$fake_pi_text" '{type:"turn_end",message:{role:"assistant",content:[{type:"text",text:$text}]}}'
      fi
      if [[ -n "${FAKE_PI_DELAY_BEFORE_AGENT_END_MS:-}" ]]; then
        sleep "$(awk "BEGIN { print ${FAKE_PI_DELAY_BEFORE_AGENT_END_MS} / 1000 }")"
      fi
      if [[ -n "${FAKE_PI_NO_AGENT_END:-}" ]]; then
        while true; do sleep 60; done
      fi
      if [[ -n "${FAKE_PI_NO_AGENT_END_MESSAGES:-}" ]]; then
        jq -cn --arg turns "$prompt_seen" '{type:"agent_end",turns:($turns|tonumber)}'
      else
        jq -cn --arg turns "$prompt_seen" '{type:"agent_end",messages:[{role:"assistant",content:"done"}],turns:($turns|tonumber)}'
      fi
      if [[ -n "${FAKE_PI_EXIT_NONZERO:-}" ]]; then
        exit 7
      fi
      ;;
    *)
      jq -cn --arg id "$id" --arg command "$type" '{id:$id,type:"response",command:$command,success:false,error:"unknown command"}'
      ;;
  esac
done

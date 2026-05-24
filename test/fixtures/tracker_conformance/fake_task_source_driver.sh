#!/bin/sh
scenario=pass
while [ $# -gt 0 ]; do
  case "$1" in
    --scenario)
      shift
      scenario="$1"
      ;;
  esac
  shift
done

state_dir="test/tmp/tracker-conformance/driver-state/$scenario"

request_id_from_input() {
  _request_id=${1#*\"request_id\":\"}
  if [ "$_request_id" = "$1" ]; then
    printf ''
    return 0
  fi
  _request_id=${_request_id%%\"*}
  printf '%s' "$_request_id"
}

write_state_file() {
  file=$1
  body=$2
  mkdir -p "$(dirname "$file")"
  printf '%s\n' "$body" >"$file"
}

append_state_file() {
  file=$1
  body=$2
  mkdir -p "$(dirname "$file")"
  printf '%s\n' "$body" >>"$file"
}

read -r input || exit 0
request_id=$(request_id_from_input "$input")
printf 'driver-diagnostic SECRET_TOKEN scenario=%s\n' "$scenario" >&2

task='{"ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
task_2='{"ref":{"backend_kind":"test-memory","remote_id":"card-2","key":"CARD-2","url":"https://tracker.example/tasks/CARD-2"},"title":"Second fake card","description":"Explicit fixture task","priority":1,"state":{"id":"doing","name":"Doing","category":"active"},"branch_hint":"card-2-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
secret_task='{"ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card SECRET_TOKEN","description":"Fixture task SECRET_TOKEN","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan SECRET_TOKEN"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
wrong_backend_task='{"ref":{"backend_kind":"wrong-backend","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
wrong_identity_task='{"ref":{"backend_kind":"test-memory","remote_id":"card-2","key":"CARD-2","url":"https://tracker.example/tasks/CARD-2"},"title":"Wrong card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-2-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
routing_task='{"ref":{"backend_kind":"test-memory","remote_id":"route-1","key":"ROUTE-1","url":"https://tracker.example/tasks/ROUTE-1"},"title":"Routing primary","description":"Routing fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"route-1-fake","labels":[{"id":null,"name":"workflow:execplan"},{"id":null,"name":"team:core"}],"blockers":[{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"}],"blockers_complete":false,"created_at":null,"updated_at":null}'
routing_task_2='{"ref":{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"},"title":"Routing blocker","description":"Routing blocker fixture task","priority":1,"state":{"id":"doing","name":"Doing","category":"active"},"branch_hint":"route-2-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
routing_missing_label='{"ref":{"backend_kind":"test-memory","remote_id":"route-1","key":"ROUTE-1","url":"https://tracker.example/tasks/ROUTE-1"},"title":"Routing primary","description":"Routing fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"route-1-fake","labels":[{"id":null,"name":"team:core"}],"blockers":[{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"}],"blockers_complete":false,"created_at":null,"updated_at":null}'
routing_nonmatching_label='{"ref":{"backend_kind":"test-memory","remote_id":"route-1","key":"ROUTE-1","url":"https://tracker.example/tasks/ROUTE-1"},"title":"Routing primary","description":"Routing fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"route-1-fake","labels":[{"id":null,"name":"workflow:other"},{"id":null,"name":"team:core"}],"blockers":[{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"}],"blockers_complete":false,"created_at":null,"updated_at":null}'
routing_wrong_blocker='{"ref":{"backend_kind":"test-memory","remote_id":"route-1","key":"ROUTE-1","url":"https://tracker.example/tasks/ROUTE-1"},"title":"Routing primary","description":"Routing fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"route-1-fake","labels":[{"id":null,"name":"workflow:execplan"},{"id":null,"name":"team:core"}],"blockers":[{"backend_kind":"test-memory","remote_id":"route-x","key":"ROUTE-X","url":"https://tracker.example/tasks/ROUTE-X"}],"blockers_complete":false,"created_at":null,"updated_at":null}'
routing_duplicate_blocker='{"ref":{"backend_kind":"test-memory","remote_id":"route-1","key":"ROUTE-1","url":"https://tracker.example/tasks/ROUTE-1"},"title":"Routing primary","description":"Routing fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"route-1-fake","labels":[{"id":null,"name":"workflow:execplan"},{"id":null,"name":"team:core"}],"blockers":[{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"},{"backend_kind":"test-memory","remote_id":"route-2","key":"ROUTE-2","url":"https://tracker.example/tasks/ROUTE-2"}],"blockers_complete":false,"created_at":null,"updated_at":null}'
transition_doing='{"task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"state":{"id":"doing","name":"Doing","category":"active"}}'
transition_done='{"task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"state":{"id":"done","name":"Done","category":"done"}}'
comment_created='{"id":"comment-created","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/comment-created","created":true}'
comment_updated='{"id":"comment-existing","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/comment-existing","created":false}'
comment_wrong_task='{"id":"comment-created","task":{"backend_kind":"test-memory","remote_id":"card-2","key":"CARD-2","url":"https://tracker.example/tasks/CARD-2"},"url":"https://tracker.example/comments/comment-created","created":true}'
comment_wrong_created='{"id":"comment-existing","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/comment-existing","created":true}'
remote_event_1='{"event_id":"event-card-1-0","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"author_id":"user-1","body":"/retry SECRET_TOKEN","command_name":"retry","excerpt":"retry excerpt SECRET_TOKEN","observed_at_ms":101}'
remote_event_2='{"event_id":"event-card-1-1","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"author_id":"user-1","body":"/status SECRET_TOKEN","command_name":"status","excerpt":"status excerpt SECRET_TOKEN","observed_at_ms":102}'
remote_event_3='{"event_id":"event-card-2-0","task":{"backend_kind":"test-memory","remote_id":"card-2","key":"CARD-2","url":"https://tracker.example/tasks/CARD-2"},"author_id":"user-2","body":"/retry SECRET_TOKEN","command_name":"retry","excerpt":"retry excerpt card-2 SECRET_TOKEN","observed_at_ms":103}'
remote_event_bad='{"event_id":"","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"author_id":"","body":"","command_name":"","excerpt":"","observed_at_ms":0}'
remote_event_oversized='{"event_id":"event-card-1-oversized","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"author_id":"user-1","body":"/retry oversize payload 1234567890 abcdefghijklmnopqrstuvwxyz 1234567890 abcdefghijklmnopqrstuvwxyz 1234567890 abcdefghijklmnopqrstuvwxyz SECRET_TOKEN","command_name":"retry","excerpt":"oversize excerpt 1234567890 abcdefghijklmnopqrstuvwxyz 1234567890 abcdefghijklmnopqrstuvwxyz 1234567890 abcdefghijklmnopqrstuvwxyz SECRET_TOKEN","observed_at_ms":104}'
ack_receipt='{"id":"ack-receipt","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/ack-receipt","created":true}'
ack_receipt_same='{"id":"ack-retry","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/ack-retry","created":true}'
ack_receipt_dupe_1='{"id":"ack-retry-1","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/ack-retry-1","created":true}'
ack_receipt_dupe_2='{"id":"ack-retry-2","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/ack-retry-2","created":true}'
ack_receipt_failure_visible='{"id":"ack-visible","task":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"url":"https://tracker.example/comments/ack-visible","created":true}'
handoff_receipt='{"reported":true}'

case "$input" in
  *'"operation":"comments.post_or_update"'*)
    mkdir -p "$state_dir/comments"
    case "$request_id" in
      req-comments-create-only)
        case "$scenario" in
          comments-bad-receipt)
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_wrong_task"
            ;;
          comments-probe-failure)
            write_state_file "$state_dir/comments/create-only.txt" "missing marker"
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_created"
            ;;
          *)
            write_state_file "$state_dir/comments/create-only.txt" '[marker comments-create-only] create-only conformance body'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_created"
            ;;
        esac
        ;;
      req-comments-update-existing)
        case "$scenario" in
          comments-bad-receipt)
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_wrong_created"
            ;;
          comments-unsupported-update)
            printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"unsupported_capability","message":"update_existing is not supported","ref":null,"capability":"comments.update"}}\n' "$request_id"
            ;;
          comments-duplicate-update)
            write_state_file "$state_dir/comments/update-existing.txt" '[marker comments-update-existing] update-existing conformance body'
            write_state_file "$state_dir/comments/update-existing-duplicate.txt" '[marker comments-update-existing] duplicate update body'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_updated"
            ;;
          *)
            write_state_file "$state_dir/comments/update-existing.txt" '[marker comments-update-existing] update-existing conformance body'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_updated"
            ;;
        esac
        ;;
      req-comments-update-missing-no-fallback)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"not_found","message":"comment id not found","ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"capability":null}}\n' "$request_id"
        ;;
      req-comments-update-missing-allow-create-fallback)
        write_state_file "$state_dir/comments/fallback-create.txt" '[marker comments-update-missing-allow-create-fallback] stale update body'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"comment":%s}}\n' "$request_id" "$comment_created"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"decode_failed","message":"unknown comments request","ref":null,"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"remote_commands.fetch_events"'*)
    case "$request_id" in
      req-remote-fetch-normalized)
        case "$scenario" in
          remote-defective)
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"events":[%s,%s]}}\n' "$request_id" "$remote_event_bad" "$remote_event_3"
            ;;
          remote-oversized-body)
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"events":[%s,%s]}}\n' "$request_id" "$remote_event_oversized" "$remote_event_3"
            ;;
          *)
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"events":[%s,%s,%s]}}\n' "$request_id" "$remote_event_1" "$remote_event_2" "$remote_event_3"
            ;;
        esac
        ;;
      req-remote-fetch-since)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"events":[%s,%s]}}\n' "$request_id" "$remote_event_2" "$remote_event_3"
        ;;
      req-remote-fetch-limit)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"events":[%s,%s]}}\n' "$request_id" "$remote_event_1" "$remote_event_3"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"decode_failed","message":"unknown remote fetch request","ref":null,"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"remote_commands.post_ack"'*)
    mkdir -p "$state_dir/remote"
    case "$request_id" in
      req-remote-ack-receipt)
        write_state_file "$state_dir/remote/ack-receipt.txt" '[marker remote-ack-receipt] ack receipt SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt"
        ;;
      req-remote-ack-retry-first)
        case "$scenario" in
          remote-ack-duplicate-visible)
            append_state_file "$state_dir/remote/ack-retry.txt" 'duplicate-visible-1 SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_dupe_1"
            ;;
          remote-ack-defective)
            write_state_file "$state_dir/remote/ack-retry.txt" 'defective duplicate mismatch SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_same"
            ;;
          *)
            write_state_file "$state_dir/remote/ack-retry.txt" 'idempotent SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_same"
            ;;
        esac
        ;;
      req-remote-ack-retry-second)
        case "$scenario" in
          remote-ack-duplicate-visible)
            append_state_file "$state_dir/remote/ack-retry.txt" 'duplicate-visible-2 SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_dupe_2"
            ;;
          remote-ack-defective)
            write_state_file "$state_dir/remote/ack-retry.txt" 'defective duplicate mismatch SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_same"
            ;;
          *)
            write_state_file "$state_dir/remote/ack-retry.txt" 'idempotent SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_same"
            ;;
        esac
        ;;
      req-remote-ack-failure-visibility)
        case "$scenario" in
          remote-defective)
            printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"permanent","message":"acknowledgement write failed SECRET_TOKEN","ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"capability":null}}\n' "$request_id"
            ;;
          *)
            write_state_file "$state_dir/remote/ack-visible.txt" '[marker remote-ack-failure-visible] ack SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"ack":%s}}\n' "$request_id" "$ack_receipt_failure_visible"
            ;;
        esac
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"decode_failed","message":"unknown remote ack request","ref":null,"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"handoff.report"'*)
    mkdir -p "$state_dir/handoff"
    case "$request_id" in
      req-handoff-claim)
        write_state_file "$state_dir/handoff/claim.txt" 'workspace/main/SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-success)
        write_state_file "$state_dir/handoff/success.txt" 'summary SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-failure)
        case "$scenario" in
          handoff-defective)
            printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"permanent","message":"handoff failure write failed SECRET_TOKEN","ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"capability":null}}\n' "$request_id"
            ;;
          *)
            write_state_file "$state_dir/handoff/failure.txt" 'reason SECRET_TOKEN'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
            ;;
        esac
        ;;
      req-handoff-park)
        write_state_file "$state_dir/handoff/park.txt" 'release policy SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-legacy-claim)
        write_state_file "$state_dir/handoff/legacy-claim.txt" 'workspace/legacy/SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-legacy-success)
        write_state_file "$state_dir/handoff/legacy-success.txt" 'legacy success SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-legacy-failure)
        write_state_file "$state_dir/handoff/legacy-failure.txt" 'legacy failure SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-legacy-park)
        write_state_file "$state_dir/handoff/legacy-park.txt" 'legacy park SECRET_TOKEN'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-claim-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-claim.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-claim.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-claim-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-claim.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-claim.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-success-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-success.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-success.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-success-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-success.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-success.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-failure-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-failure.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-failure.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-failure-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-failure.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-failure.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-park-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-park.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-park.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-park-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-park.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-park.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-claim-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-claim.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-claim.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-claim-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-claim.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-claim.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-success-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-success.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-success.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-success-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-success.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-success.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-failure-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-failure.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-failure.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-failure-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-failure.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-failure.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-park-first)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-park.txt" 'duplicate-visible-1 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-park.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      req-handoff-retry-legacy-park-second)
        case "$scenario" in
          handoff-duplicate-visible)
            append_state_file "$state_dir/handoff/retry-legacy-park.txt" 'duplicate-visible-2 SECRET_TOKEN'
            ;;
          *)
            write_state_file "$state_dir/handoff/retry-legacy-park.txt" 'idempotent SECRET_TOKEN'
            ;;
        esac
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"handoff":%s}}\n' "$request_id" "$handoff_receipt"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"decode_failed","message":"unknown handoff request","ref":null,"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"state_transitions.transition"'*)
    mkdir -p "$state_dir"
    case "$request_id" in
      req-state-transition-target-id)
        write_state_file "$state_dir/current-state.txt" 'doing'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"transition":%s}}\n' "$request_id" "$transition_doing"
        ;;
      req-state-transition-target-name-only)
        write_state_file "$state_dir/current-state.txt" 'done'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"transition":%s}}\n' "$request_id" "$transition_done"
        ;;
      req-state-transition-blank-target)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"permanent","message":"target state name was blank","ref":null,"capability":null}}\n' "$request_id"
        ;;
      req-state-transition-unknown-target)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"permanent","message":"unknown target state","ref":null,"capability":null}}\n' "$request_id"
        ;;
      req-state-transition-reason-propagation)
        case "$scenario" in
          state-reason-missing)
            printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"permanent","message":"reason marker was not preserved","ref":null,"capability":null}}\n' "$request_id"
            ;;
          *)
            write_state_file "$state_dir/current-state.txt" 'doing'
            write_state_file "$state_dir/reason.txt" '[marker state-reason-propagation] verify reason persistence'
            printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"transition":%s}}\n' "$request_id" "$transition_doing"
            ;;
        esac
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"decode_failed","message":"unknown state transition request","ref":null,"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"task_source.fetch_candidates"'*)
    case "$scenario" in
      fetch-wrong-backend)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$wrong_backend_task"
        ;;
      routing-pass|routing-wrong-blocker-ref|routing-duplicate-blocker-ref)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_task" "$routing_task_2"
        ;;
      routing-missing-label)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_missing_label" "$routing_task_2"
        ;;
      routing-nonmatching-label)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_nonmatching_label" "$routing_task_2"
        ;;
      explicit-fixtures)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$task_2"
        ;;
      remote-pass|remote-ack-duplicate-visible|remote-ack-defective|remote-defective|remote-oversized-body|remote-probe-failure|remote-cleanup-failure|handoff-pass|handoff-duplicate-visible|handoff-defective|handoff-probe-failure|handoff-cleanup-failure)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$task" "$task_2"
        ;;
      secret-transcripts)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$secret_task"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$task"
        ;;
    esac
    ;;
  *'"operation":"task_source.refresh_by_refs"'*'"backend_kind":"wrong-backend"'*)
    case "$scenario" in
      wrong-backend-bad)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$task"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":false,"error":{"kind":"not_found","message":"wrong backend ref rejected","ref":{"backend_kind":"wrong-backend","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"capability":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"task_source.refresh_by_refs"'*)
    case "$scenario" in
      unstable-identity)
        bad_task='{"ref":{"backend_kind":"test-memory","remote_id":"card-1-changed","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$bad_task"
        ;;
      routing-pass|routing-missing-label|routing-nonmatching-label)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_task" "$routing_task_2"
        ;;
      routing-wrong-blocker-ref)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_wrong_blocker" "$routing_task_2"
        ;;
      routing-duplicate-blocker-ref)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$routing_duplicate_blocker" "$routing_task_2"
        ;;
      explicit-fixtures)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$task_2"
        ;;
      remote-pass|remote-ack-duplicate-visible|remote-ack-defective|remote-defective|remote-oversized-body|remote-probe-failure|remote-cleanup-failure|handoff-pass|handoff-duplicate-visible|handoff-defective|handoff-probe-failure|handoff-cleanup-failure)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s,%s]}}\n' "$request_id" "$task" "$task_2"
        ;;
      secret-transcripts)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$secret_task"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$task"
        ;;
    esac
    ;;
  *'"operation":"task_source.lookup_by_operator_ref"'*'"operator_ref":"   "'*)
    case "$scenario" in
      lookup-empty-matches)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$task"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":null}}\n' "$request_id"
        ;;
    esac
    ;;
  *'"operation":"task_source.lookup_by_operator_ref"'*)
    case "$scenario" in
      lookup-known-wrong)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$wrong_identity_task"
        ;;
      explicit-fixtures)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$task_2"
        ;;
      remote-pass|remote-ack-duplicate-visible|remote-ack-defective|remote-defective|remote-oversized-body|remote-probe-failure|remote-cleanup-failure|handoff-pass|handoff-duplicate-visible|handoff-defective|handoff-probe-failure|handoff-cleanup-failure)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$task"
        ;;
      routing-pass|routing-missing-label|routing-wrong-blocker-ref|routing-nonmatching-label|routing-duplicate-blocker-ref)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$routing_task"
        ;;
      secret-transcripts)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$secret_task"
        ;;
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$task"
        ;;
    esac
    ;;
  *)
    printf '{"schema_version":1,"request_id":"unknown","ok":false,"error":{"kind":"decode_failed","message":"unknown request","ref":null,"capability":null}}\n'
    ;;
esac

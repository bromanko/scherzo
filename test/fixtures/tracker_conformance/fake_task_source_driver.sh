#!/bin/sh
scenario=pass
mode=serve
while [ $# -gt 0 ]; do
  case "$1" in
    --mode)
      shift
      mode="$1"
      ;;
    --scenario)
      shift
      scenario="$1"
      ;;
  esac
  shift
done

request_id_from_input() {
  _request_id=${1#*\"request_id\":\"}
  if [ "$_request_id" = "$1" ]; then
    printf ''
    return 0
  fi
  _request_id=${_request_id%%\"*}
  printf '%s' "$_request_id"
}

read -r input || exit 0
request_id=$(request_id_from_input "$input")
printf 'driver-diagnostic SECRET_TOKEN scenario=%s mode=%s\n' "$scenario" "$mode" >&2
task='{"ref":{"backend_kind":"test-memory","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
wrong_backend_task='{"ref":{"backend_kind":"wrong-backend","remote_id":"card-1","key":"CARD-1","url":"https://tracker.example/tasks/CARD-1"},"title":"Fake card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-1-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
wrong_identity_task='{"ref":{"backend_kind":"test-memory","remote_id":"card-2","key":"CARD-2","url":"https://tracker.example/tasks/CARD-2"},"title":"Wrong card","description":"Fixture task","priority":2,"state":{"id":"todo","name":"Todo","category":"ready"},"branch_hint":"card-2-fake","labels":[{"id":null,"name":"workflow:execplan"}],"blockers":[],"blockers_complete":true,"created_at":null,"updated_at":null}'
case "$input" in
  *'"operation":"task_source.fetch_candidates"'*)
    case "$scenario" in
      fetch-wrong-backend)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[%s]}}\n' "$request_id" "$wrong_backend_task"
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
      *)
        printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"task":%s}}\n' "$request_id" "$task"
        ;;
    esac
    ;;
  *)
    printf '{"schema_version":1,"request_id":"unknown","ok":false,"error":{"kind":"decode_failed","message":"unknown request","ref":null,"capability":null}}\n'
    ;;
esac

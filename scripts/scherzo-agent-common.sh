#!/usr/bin/env sh

fail() {
  echo "scherzo-agent: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || fail "required command not found: $1"
}

require_local_commands() {
  require_command gh
  require_command git
  require_command jj
  require_command curl
}

status_configured() {
  name=$1
  value=$2
  if [ -n "$value" ]; then
    echo "$name=configured"
  else
    echo "$name=missing"
  fi
}

require_var() {
  name=$1
  value=$2
  if [ -z "$value" ]; then
    fail "$name is required; add it to .env.local or export it before running this script"
  fi
}

toml_escape() {
  printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'
}

require_single_line() {
  name=$1
  value=$2
  lines=$(printf '%s\n' "$value" | wc -l | tr -d ' ')
  if [ "$lines" != "1" ]; then
    fail "$name must not contain newlines"
  fi
}

write_jj_config() {
  require_var "SCHERZO_AGENT_GIT_EMAIL" "$SCHERZO_AGENT_GIT_EMAIL"
  require_single_line "SCHERZO_AGENT_GIT_NAME" "$SCHERZO_AGENT_GIT_NAME"
  require_single_line "SCHERZO_AGENT_GIT_EMAIL" "$SCHERZO_AGENT_GIT_EMAIL"
  mkdir -p "$SCHERZO_REPO_ROOT/.scherzo"
  escaped_name=$(toml_escape "$SCHERZO_AGENT_GIT_NAME")
  escaped_email=$(toml_escape "$SCHERZO_AGENT_GIT_EMAIL")
  {
    printf '[user]\n'
    printf 'name = "%s"\n' "$escaped_name"
    printf 'email = "%s"\n' "$escaped_email"
  } > "$JJ_AGENT_CONFIG"
}

prepare_agent_env() {
  if [ -z "$SCHERZO_AGENT_PR_REMOTE" ]; then
    SCHERZO_AGENT_PR_REMOTE=scherzo-agent
  fi
  if [ -z "$SCHERZO_AGENT_PR_REPO" ]; then
    SCHERZO_AGENT_PR_REPO=bromanko/scherzo
  fi
  if [ -z "$SCHERZO_AGENT_GIT_NAME" ]; then
    SCHERZO_AGENT_GIT_NAME="Scherzo Agent"
  fi
  if [ -z "$SCHERZO_AGENT_SSH_HOST" ]; then
    SCHERZO_AGENT_SSH_HOST=github-scherzo-agent
  fi
  if [ -z "$SCHERZO_REPO_ROOT" ]; then
    if [ -n "$DEVENV_ROOT" ]; then
      SCHERZO_REPO_ROOT=$DEVENV_ROOT
    else
      SCHERZO_REPO_ROOT=$PWD
    fi
  fi
  cd "$SCHERZO_REPO_ROOT"

  GH_CONFIG_DIR=$SCHERZO_REPO_ROOT/.scherzo/gh-agent
  JJ_AGENT_CONFIG=$SCHERZO_REPO_ROOT/.scherzo/jj-agent.toml
  mkdir -p "$SCHERZO_REPO_ROOT/.scherzo" "$GH_CONFIG_DIR"

  if [ -n "$SCHERZO_AGENT_GITHUB_TOKEN" ]; then
    GH_TOKEN=$SCHERZO_AGENT_GITHUB_TOKEN
    GITHUB_TOKEN=$SCHERZO_AGENT_GITHUB_TOKEN
    export GH_TOKEN GITHUB_TOKEN
  else
    unset GH_TOKEN
    unset GITHUB_TOKEN
  fi

  if [ -n "$SCHERZO_AGENT_LINEAR_API_KEY" ]; then
    LINEAR_API_KEY=$SCHERZO_AGENT_LINEAR_API_KEY
    export LINEAR_API_KEY
  else
    unset LINEAR_API_KEY
  fi

  SCHERZO_PR_REMOTE=$SCHERZO_AGENT_PR_REMOTE
  SCHERZO_PR_REPO=$SCHERZO_AGENT_PR_REPO
  GIT_AUTHOR_NAME=$SCHERZO_AGENT_GIT_NAME
  GIT_AUTHOR_EMAIL=$SCHERZO_AGENT_GIT_EMAIL
  GIT_COMMITTER_NAME=$SCHERZO_AGENT_GIT_NAME
  GIT_COMMITTER_EMAIL=$SCHERZO_AGENT_GIT_EMAIL

  if [ -n "$JJ_CONFIG" ]; then
    JJ_CONFIG=$JJ_CONFIG:$JJ_AGENT_CONFIG
  else
    JJ_CONFIG=$JJ_AGENT_CONFIG
  fi

  unset GIT_SSH_COMMAND

  export GH_CONFIG_DIR JJ_AGENT_CONFIG JJ_CONFIG SCHERZO_REPO_ROOT
  export SCHERZO_AGENT_PR_REMOTE SCHERZO_AGENT_PR_REPO SCHERZO_AGENT_GIT_NAME SCHERZO_AGENT_SSH_HOST
  export SCHERZO_PR_REMOTE SCHERZO_PR_REPO
  export GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL
}

remote_url() {
  remote_name=$1
  jj_url=""
  if command -v jj >/dev/null 2>&1; then
    jj_url=$(jj git remote list --color=never 2>/dev/null | while IFS=' ' read -r name url rest; do
      if [ "$name" = "$remote_name" ]; then
        printf '%s\n' "$url"
        break
      fi
    done)
  fi
  if [ -n "$jj_url" ]; then
    printf '%s\n' "$jj_url"
    return 0
  fi
  git remote get-url "$remote_name" 2>/dev/null
}

remote_host() {
  url=$1
  printf '%s\n' "$url" | sed -n 's#^git@\([^:][^:]*\):.*#\1#p; s#^ssh://git@\([^/][^/]*\)/.*#\1#p; s#^ssh://\([^/@][^/@]*\)/.*#\1#p' | head -n 1
}

require_agent_remote() {
  remote_name=$SCHERZO_PR_REMOTE
  url=$(remote_url "$remote_name" || true)
  if [ -z "$url" ]; then
    fail "remote '$remote_name' was not found; add it with: jj git remote add $remote_name git@$SCHERZO_AGENT_SSH_HOST:$SCHERZO_PR_REPO.git"
  fi
  case "$url" in
    git@*:*) ;;
    ssh://*) ;;
    *)
      echo "scherzo-agent: remote '$remote_name' is '$url'" >&2
      fail "remote '$remote_name' must be SSH-based and use host '$SCHERZO_AGENT_SSH_HOST'; add or update the agent remote instead of changing origin"
      ;;
  esac
  host=$(remote_host "$url")
  if [ "$host" != "$SCHERZO_AGENT_SSH_HOST" ]; then
    echo "scherzo-agent: remote '$remote_name' is '$url'" >&2
    fail "remote '$remote_name' uses host '$host', expected '$SCHERZO_AGENT_SSH_HOST'"
  fi
  printf '%s\n' "$url"
}

show_identities() {
  echo "git author: $(git var GIT_AUTHOR_IDENT)"
  echo "git committer: $(git var GIT_COMMITTER_IDENT)"
  echo "jj user.name: $(jj config get user.name)"
  echo "jj user.email: $(jj config get user.email)"
}

print_local_environment() {
  echo "Scherzo agent local environment check"
  echo "SCHERZO_PR_REMOTE=$SCHERZO_PR_REMOTE"
  echo "SCHERZO_PR_REPO=$SCHERZO_PR_REPO"
  echo "SCHERZO_REPO_ROOT=$SCHERZO_REPO_ROOT"
  echo "SCHERZO_AGENT_GIT_NAME=$SCHERZO_AGENT_GIT_NAME"
  echo "SCHERZO_AGENT_GIT_EMAIL=$SCHERZO_AGENT_GIT_EMAIL"
  echo "SCHERZO_AGENT_SSH_HOST=$SCHERZO_AGENT_SSH_HOST"
  status_configured "SCHERZO_AGENT_GITHUB_TOKEN" "$SCHERZO_AGENT_GITHUB_TOKEN"
  status_configured "SCHERZO_AGENT_GITHUB_LOGIN" "$SCHERZO_AGENT_GITHUB_LOGIN"
  status_configured "SCHERZO_AGENT_LINEAR_API_KEY" "$SCHERZO_AGENT_LINEAR_API_KEY"
  status_configured "GH_TOKEN" "$GH_TOKEN"
  status_configured "GITHUB_TOKEN" "$GITHUB_TOKEN"
  status_configured "LINEAR_API_KEY" "$LINEAR_API_KEY"
  if [ -n "$GIT_SSH_COMMAND" ]; then
    echo "GIT_SSH_COMMAND=configured"
  else
    echo "GIT_SSH_COMMAND=unset"
  fi
  echo "GH_CONFIG_DIR=$GH_CONFIG_DIR"
  echo "JJ_CONFIG=$JJ_CONFIG"
  echo "gh=$(command -v gh)"
  echo "jj=$(command -v jj)"
  echo "curl=$(command -v curl)"
}

require_git_email_for_identity_checks() {
  if [ -z "$SCHERZO_AGENT_GIT_EMAIL" ]; then
    echo "scherzo-agent: SCHERZO_AGENT_GIT_EMAIL is required for git and jj identity checks" >&2
    exit 1
  fi
}

require_live_identity() {
  require_command gh
  require_command git
  require_command jj
  require_command curl
  require_command jq
  require_command ssh
  require_var "SCHERZO_AGENT_GITHUB_TOKEN" "$SCHERZO_AGENT_GITHUB_TOKEN"
  require_var "SCHERZO_AGENT_GITHUB_LOGIN" "$SCHERZO_AGENT_GITHUB_LOGIN"
  require_var "SCHERZO_AGENT_LINEAR_API_KEY" "$SCHERZO_AGENT_LINEAR_API_KEY"
  require_var "SCHERZO_AGENT_GIT_EMAIL" "$SCHERZO_AGENT_GIT_EMAIL"
  write_jj_config
  require_agent_remote >/dev/null
}

require_github_identity() {
  gh_login=$(gh api user --jq .login 2>/dev/null || true)
  if [ -z "$gh_login" ]; then
    fail "GitHub token check failed; verify SCHERZO_AGENT_GITHUB_TOKEN has API access"
  fi
  if [ "$gh_login" != "$SCHERZO_AGENT_GITHUB_LOGIN" ]; then
    fail "GitHub token login '$gh_login' does not match SCHERZO_AGENT_GITHUB_LOGIN '$SCHERZO_AGENT_GITHUB_LOGIN'"
  fi
}

require_github_repo_access() {
  repo_full_name=$(gh api "repos/$SCHERZO_PR_REPO" --jq .full_name 2>/dev/null || true)
  if [ "$repo_full_name" != "$SCHERZO_PR_REPO" ]; then
    fail "GitHub token cannot read repository '$SCHERZO_PR_REPO'"
  fi
}

require_linear_identity() {
  linear_response=$(curl -fsS -X POST https://api.linear.app/graphql \
    -H "Authorization: $LINEAR_API_KEY" \
    -H "Content-Type: application/json" \
    --data '{"query":"query { viewer { id name email } }"}' 2>/dev/null || true)
  if [ -z "$linear_response" ]; then
    fail "Linear viewer check failed; verify SCHERZO_AGENT_LINEAR_API_KEY"
  fi
  if ! printf '%s' "$linear_response" | jq -e '.errors == null and .data.viewer != null' >/dev/null 2>&1; then
    fail "Linear viewer check was unauthorized or returned an unexpected response"
  fi
  linear_actor=$(printf '%s' "$linear_response" | jq -r '.data.viewer.name // .data.viewer.email // .data.viewer.id')
}

require_ssh_identity() {
  ssh_output=$(ssh -T -o BatchMode=yes "git@$SCHERZO_AGENT_SSH_HOST" 2>&1 || true)
  if ! printf '%s\n' "$ssh_output" | grep -F "successfully authenticated" >/dev/null \
    || ! printf '%s\n' "$ssh_output" | grep -F "$SCHERZO_AGENT_GITHUB_LOGIN" >/dev/null; then
    printf '%s\n' "$ssh_output" >&2
    fail "SSH authentication through '$SCHERZO_AGENT_SSH_HOST' did not match '$SCHERZO_AGENT_GITHUB_LOGIN'"
  fi
}

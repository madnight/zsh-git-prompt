#!/usr/bin/env bats
# NB: BATS will fail any test if any command within returns a non-zero exit
# To surpress for commands EXPECTED to fail do:
#       cmd_fails || true
BATS_IN=/tmp/bats_in
BATS_OUT=/tmp/bats_out
TEST_D=/tmp/zsh-git-bats
TEST_DUP=/tmp/zsh-git-bats_upstream

# This is the absolution path to gitstatus binary to test, override with BATS_IN
EXE="$(dirname $BATS_TEST_DIRNAME)/.bin/gitstatus"
if [ -f $BATS_IN ]; then
    . "$BATS_IN"
fi

# Inside tests, STDOUT/ERR captured. Use this to print for later inspection
bats_out() {
    echo "$@" >> "$BATS_OUT"
}

add_commit () {
    echo "Hello world" >> "$1"
    git add "$1"
    git commit -m "$1 commit"
}

add_commit_text () {
    echo "$2" >> "$1"
    git add "$1"
    git commit -m "$2 commit"
}

setup() {
    command rm -rf "$TEST_D" "$TEST_DUP"
    command mkdir -p "$TEST_D"
    command cd "$TEST_D"
    git init
    git config user.email 'you@example.com'
    git config user.name 'Your Name'
}

teardown() {
    command rm -rf "$TEST_D" "$TEST_DUP"
}

@test "Nullify output" {
    echo "" > "$BATS_OUT"
}

@test "Empty directory" {
    rm -rf .git

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" = "" ]
}

@test "Initial repo" {
    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 0 0 0 0 0 0 0 1 .. 0 0" ]
}

@test "Local branch indicator" {
    add_commit "first"

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 0 0 0 0 0 0 0 1 .. 0 0" ]
}

@test "Detached HEAD, on hash" {
    add_commit "first"
    add_commit "second"
    git checkout HEAD~1
    local actual_hash="$(git rev-parse --short HEAD)"

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == ":$actual_hash 0 0 0 0 0 0 0 0 .. 0 0" ]
}

@test "Basic stats, NO conflicts" {
    add_commit "first"
    echo "first line" >> first
    git stash

    echo "first line" >> first
    echo "second line" >> second
    echo "third line" >> third
    touch untracked1 untracked2
    git add first second third

    echo "first line" >> first
    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"
    git branch --set-upstream-to=up/master

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 0 0 3 0 1 2 1 0 up/master 0 0" ]
}

@test "Basic stats, ONLY conflicts" {
    add_commit "first"
    git branch dev
    git checkout dev
    add_commit "first"

    git checkout master
    echo "A different line" >> first
    git add first
    git commit -m "A different line"
    git merge dev || true

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 0 0 0 1 0 0 0 1 .. 1 0" ]
}

@test "Remote branch, 1 ahead" {
    add_commit "first"
    add_commit "first"

    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"
    git branch --set-upstream-to=up/master

    add_commit_text "first" "Local text 1"

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 1 0 0 0 0 0 0 0 up/master 0 0" ]
}

@test "Remote branch, 2 behind" {
    add_commit "first"
    add_commit "first"
    add_commit_text "first" "Remote text 1"
    add_commit_text "first" "Remote text 2"

    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"
    git branch --set-upstream-to=up/master

    git reset --hard HEAD~2

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 0 2 0 0 0 0 0 0 up/master 0 0" ]
}

@test "Remote branch, 1 ahead & 2 behind" {
    add_commit "first"
    add_commit "first"
    add_commit_text "first" "Remote text 1"
    add_commit_text "first" "Remote text 2"

    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"
    git branch --set-upstream-to=up/master

    git reset --hard HEAD~2
    add_commit_text "first" "Local text 1"

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "master 1 2 0 0 0 0 0 0 up/master 0 0" ]
}

@test "Remote branch, gone" {
    skip "Fails for Haskell at this time."
    add_commit "first"
    add_commit_text "first" "second line"

    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"

    git branch dev
    git checkout dev
    git push -u up dev
    git fetch up
    git push up :dev

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "dev 0 0 0 0 0 0 0 0 up/dev 0 0" ]
}

@test "Read from STDIN" {
    add_commit "first"
    echo "first line" >> first
    git stash

    echo "first line" >> first
    echo "second line" >> second
    echo "third line" >> third
    touch untracked1 untracked2
    git add first second third

    echo "first line" >> first
    cp -r "$TEST_D" "$TEST_DUP"
    git remote add -f up "$TEST_DUP"
    git branch --set-upstream-to=up/master

    [ "$(git status --branch --porcelain | "$EXE")" == "master 0 0 3 0 1 2 1 0 up/master 0 0" ]
}

@test "Merge in progress" {
    add_commit "first"

    git branch dev
    git checkout dev
    add_commit_text "first" "commit on dev"

    git checkout master
    add_commit_text "first" "commit on master"

    git checkout dev
    git merge master || true

    run $EXE
    [ "$status" -eq 0 ]
    [ "$output" == "dev 0 0 0 1 0 0 0 1 .. 1 0" ]
}

@test "Rebase in progress" {
    add_commit "first"

    git branch dev
    git checkout dev
    add_commit_text "first" "only on dev 1"
    add_commit_text "first" "only on dev 2"

    git checkout master
    add_commit_text "first" "only on master 1"
    add_commit_text "first" "only on master 2"

    git checkout dev
    git rebase master || true

    run $EXE
    [ "$status" -eq 0 ]
    local actual_hash="$(git rev-parse --short HEAD)"
    [ "$output" == ":$actual_hash 0 0 0 1 0 0 0 0 .. 0 1/2" ]
}

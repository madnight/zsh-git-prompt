#!/usr/bin/env bats

setup () {
  rm -rf /tmp/zsh-git-prompt/haskell-tests
  mkdir -p /tmp/zsh-git-prompt/haskell-tests
  cd /tmp/zsh-git-prompt/haskell-tests
  git init
  git config user.email 'you@example.com'
  git config user.name 'Your Name'
}

add_commit () {
  echo "Hello World" > "$1"
  git add "$1"
  git commit -m "$1 commit"
}

@test "non git dir" {
  rm -rf .git
  run gitstatus
  [ "$status" -eq 0 ]
  [ "$(gitstatus | head -c1 | wc -c)" -eq 0 ] # empty output
}

@test "empty repo" {
  run gitstatus
  [ "$status" -eq 0 ]
}

@test "1 commit" {
  add_commit "first"
  [ "$(gitstatus)" == "master 0 0 0 0 0 0 0 1 .. 0 0" ]
}


@test "3 local commits, 3 upstream commits, 1 diverged" {
  add_commit "first"
  add_commit "second"
  git branch tree
  git checkout tree
  add_commit "third"
  git checkout master
}

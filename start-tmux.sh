#!/usr/bin/env bash

tmux new-session -d -s 'control-plane'
tmux send-keys 'cabal exec -- control-plane-server start' 'C-m'
tmux rename-window 'server'
tmux select-window -t control-plane:0
tmux split-window -h 'cabal exec -- control-plane-jobs'
tmux -2 attach-session -t control-plane

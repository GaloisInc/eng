#!/usr/bin/env nix
#! nix shell nixpkgs#swi-prolog -c bash

# swipl -q -t main src/top.pl "$@"
swipl -t main src/top.pl "$@"

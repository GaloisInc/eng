#!/usr/bin/env bash

if type -p nix > /dev/null ; then
    nix run nixpkgs#swi-prolog -- -t main src/top.pl "$@"
else
    # assumes SWI Prolog is available in the user's path
    # swipl -q -t main src/top.pl "$@"
    swipl -t main src/top.pl "$@"
fi

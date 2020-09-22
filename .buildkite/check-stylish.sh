#!/usr/bin/env bash

set -euo pipefail

echo "Stylish Haskell Configuration"
echo "============================="
echo
cat .stylish-haskell.yaml
echo
echo

echo "Running Stylish Haskell"
echo "======================="
echo
stylish-haskell -c .stylish-haskell.yaml -i $(git ls-files -- '*.hs' | grep 'bm-timeline')

echo
echo
echo "Detect Change in source code"
git status -uno

git diff --exit-code

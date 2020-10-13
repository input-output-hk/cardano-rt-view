#!/usr/bin/env bash

cabal_project="$(git rev-parse --show-toplevel)"/cabal.project

set -x
sed -i 's_^    -- ../_    ../_' "$cabal_project"
sed -ni '1,/--- 8< ---/ p'      "$cabal_project"

#!/usr/bin/env bash

set -e

REF_TO_UPDATE_TO=HEAD
PROJECT_ROOT=$(git rev-parse --show-toplevel)
CABAL_PROJECT_BASE="${PROJECT_ROOT}/cabal.project.base"

REV_OLD=$(grep -C 2 'github.com/well-typed/libclang-bindings' "${CABAL_PROJECT_BASE}" |
  grep 'tag:' |
  awk '{print $NF}')

if [ -z "$REV_OLD" ]; then
  echo "Error: Could not find current libclang-bindings tag in $CABAL_PROJECT_BASE"
  exit 1
else
  echo "Old libclang-bindings revision: ${REV_OLD}"
fi

REV_NEW=$(git ls-remote https://github.com/well-typed/libclang-bindings ${REF_TO_UPDATE_TO} | cut -f 1)
echo "New libclang-bindings revision: ${REV_NEW}"

sed -i 's/'"${REV_OLD}"'/'"${REV_NEW}"'/' "$CABAL_PROJECT_BASE"

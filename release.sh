#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

rm -rf dist-newstyle
rm -rf .ghc.env*

cabal sdist
cabal v2-haddock prettyprinter-interp \
  --haddock-for-hackage \
  --haddock-hyperlinked-source \
  --enable-documentation

SDIST=$(find . -name 'prettyprinter-interp-*.tar.gz' | grep -v docs)
DDIST=$(find . -name 'prettyprinter-interp-*.tar.gz' | grep docs)

echo "To publish a release candidate, run:"
echo "  cabal upload ${SDIST}"
echo "  cabal upload --documentation ${DDIST}"
echo ""
echo "To make a release, run:"
echo "  cabal upload --publish ${SDIST}"
echo "  cabal upload --publish --documentation ${DDIST}"

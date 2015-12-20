#!/usr/bin/env bash
set -e

if [ ! -d "reflex-dom" ]; then
    git clone -b develop https://github.com/ryantrinkle/reflex-dom.git
fi
sed -i.bak -e "s/bifunctors == 4\.2\.\*/bifunctors >= 4\.2/g" reflex-dom/reflex-dom.cabal

stack build

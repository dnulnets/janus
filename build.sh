#!/usr/bin/env bash
cd ui
spago build
spago bundle-app
cp index.html ../api/www/index.html
cp index.js ../api/www/index.js
cd ..
cd api
stack build


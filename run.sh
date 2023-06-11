#!/usr/bin/env bash
set -o errexit
echo "Building UI frontend"
cd ui
spago build
spago bundle-app
echo "Copying UI frontend to API backend"
cp index.html ../api/www/index.html
cp index.js ../api/www/index.js
cd ..
echo "Building API backend"
cd api
stack build
cd ..
echo "Running the application"
cd api
stack exec api-exe
cd ..


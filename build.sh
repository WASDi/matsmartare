#!/bin/bash
echo; echo "##### CLEAN"
rm -r server/build/ 2> /dev/null
rm -r client/build/ 2> /dev/null

echo; echo "##### BUILD SERVER"
npm run --prefix server build

echo; echo "##### BUILD CLIENT"
npm run --prefix client build

echo "##### DONE"
mv client/build/ server/build/client

echo "Start with:"
echo "node server/build/server.js"

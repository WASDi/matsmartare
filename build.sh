#!/bin/bash

build_client=false
build_server=false

case "$1" in
  server)
    build_server=true
    ;;

  client)
    build_client=true
    ;;

  both)
    build_server=true
    build_client=true
    ;;

  *)
    echo $"Usage: $0 {server|client|both}"
    exit 1
esac

echo #####

echo "##### CLEAN"; echo
if [ "$build_client" = true ] ; then
  rm -r client/build/ 2> /dev/null
fi
if [ "$build_server" = true ] ; then
  rm -r server/build/ 2> /dev/null
fi

echo #####

if [ "$build_client" = true ] ; then
  echo "##### BUILD CLIENT"
  npm run --prefix client build
  if [ "$?" -ne "0" ]; then
    echo; echo "##### BUILD CLIENT FAILED"
    exit 1
  fi
fi
if [ "$build_server" = true ] ; then
  echo "##### BUILD SERVER"
  npm run --prefix server build
  if [ "$?" -ne "0" ]; then
    echo; echo "##### BUILD SERVER FAILED"
    exit 1
  fi
fi

echo #####

if [ "$build_client" = true ] ; then
  echo "##### CLIENT needs manual deploy"; echo
fi
if [ "$build_server" = true ] ; then
  echo "##### DEPLOY SERVER"; echo
  scp -r server/build/ rpi:/home/wasd/matsmartare/server_bin
fi

echo #####

echo "##### DONE"

#!/bin/bash

set -e

cd "$(dirname "$0")"

./.venv/bin/python fetch_items.py >> log.fetch.txt 2>&1
./.venv/bin/python generate.py >> log.generate.txt 2>&1
cp data/everything.json /var/www/everything.json

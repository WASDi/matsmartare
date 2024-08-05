#!/bin/bash

set -e

cd "$(dirname "$0")"

./.venv/bin/python fetch_items.py >> log.txt 2>&1

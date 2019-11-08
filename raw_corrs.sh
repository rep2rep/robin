#!/usr/bin/env bash

cat "$1" | awk 'BEGIN {FS=", "}; /^\(.*\)$/ {print $1 " ~> " $2}' | cut -c 2- | sort | uniq

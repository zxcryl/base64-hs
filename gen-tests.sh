#!/usr/bin/env bash

mkdir -p files
rm -f files/*.b64
if [ ! -s files/ ]; then
    echo "No input files" >&2
    exit 1
fi

exit_code=0

for file in files/*; do
    basename=${file##*/}
    echo -n "Generating $basename.b64 output... "
    if base64 -w0 < "$file" > files/"$basename.b64"; then
        echo "ok"
    else
        exit_code=1
    fi
done

exit $exit_code

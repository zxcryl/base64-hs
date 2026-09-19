#!/usr/bin/env bash

if ! ghc base64.hs; then
    echo "Failed to compile"
    exit 1
fi

tmpfile="/tmp/base64-hs.testfile.$$"

touch "$tmpfile"

cleanup() {
    rm -f "$tmpfile"
}

trap cleanup EXIT

exit_code=0

for file in files/*.b64; do
    basename=${file%.b64}

    echo -n "Testing encode $basename... "
    if ./base64 < "$basename" > "$tmpfile" && 
        diff -w "$file" "$tmpfile"; 
    then
        echo -e "\e[32m" "ok"
    else
        exit_code=1
        echo -e "\e[31m" "fail"
    fi
    printf "\e[0m"

    echo -n "Testing decode $file... "
    if ./base64 -d < "$file" > "$tmpfile" && 
        cmp -s "$basename" "$tmpfile"; 
    then
        echo -e "\e[32m" "ok"
    else
        exit_code=1
        echo -e "\e[31m" "fail"
    fi
    printf "\e[0m"
done

exit $exit_code

#!/bin/bash

if [[ $@ ]]
then
    # Get data from args
    data="$@"
else
    # Get data from STDIN
    data=$(cat)
fi

if [[ -z $data ]]
then
    # No data; quit
    exit 1
fi

# Fix protocol
data=$(sed 's|^org-protocol-html://|org-protocol://|' <<<"$data")

# Split data
readarray -t data <<<"$(sed -r 's|^(org-protocol://capture://w/[^/]+/[^/]+/)(.*)|\1\n\2|' <<<"$data")"

start="${data[0]}"
end="${data[1]}"

# Decode URL-encoded/quoted data
end=$(python -c "import sys, urllib; print urllib.unquote(' '.join(sys.argv[1:]))" "$end")

# Convert with Pandoc
end=$(pandoc --no-wrap -f html -t org <<<"$end")

# Reencode data
end=$(python -c "import sys, urllib; print urllib.quote(' '.join(sys.argv[1:]), safe='')" "$end")

# Send to Emacs
emacsclient "${start}${end}"

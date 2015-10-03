#!/bin/bash

# ** Defaults
heading="Heading"
template="w"
url="http://example.com"

# ** Functions
function urlencode {
    python -c "import sys, urllib; print urllib.quote(' '.join(sys.argv[1:]), safe='')" "$@"
}
 function usage {
     cat <<EOF
org-protocol-capture-html [-t TITLE] [-u URL] [HTML]

Send HTML to Emacs through org-protocol, passing it through Pandoc to
convert HTML to Org-mode.  HTML may be passed as an argument or
through STDIN.

Options:
    -h HEADING   Use HEADING as the Org heading (default: w)
    -t TEMPLATE  Use the org-capture template with TEMPLATE key
    -u URL       Use URL for the heading link
EOF
}

# ** Args
while getopts "h:t:u:" opt
do
    case $opt in
        h) heading=$OPTARG ;;
        t) template=$OPTARG ;;
        u) url=$OPTARG ;;
        *) usage; exit ;;
    esac
done
shift $(( OPTIND - 1 ));

# ** Get HTML
if [[ $@ ]]
then
    # Get from args
    data="$@"
else
    # Get from STDIN
    data=$(cat)
fi

if ! [[ $data ]]
then
    # No data; quit
    echo "No data passed via args or STDIN." >&2
    exit 1
fi

# ** Check template length
if [[ ${#template} -gt 1 ]]
then
    echo "Template key should be one letter." >&2
    exit 1
fi

# ** URL-encode data
heading=$(urlencode "$heading")
url=$(urlencode "$url")
data=$(urlencode "$data")

# ** Send to Emacs
emacsclient "org-protocol://capture-html://$template/$url/$heading/$data"

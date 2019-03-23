#!/bin/bash

# * Defaults

heading=" "
protocol="capture-html"
template="w"

# * Functions

function debug {
    if [[ -n $debug ]]
    then
        function debug {
            echo "DEBUG: $@" >&2
        }
        debug "$@"
    else
        function debug {
            true
        }
    fi
}
function die {
    echo "$@" >&2
    exit 1
}
function usage {
    cat <<EOF
$0 [OPTIONS] [HTML]
html | $0 [OPTIONS]

Send HTML to Emacs through org-protocol, passing it through Pandoc to
convert HTML to Org-mode.  HTML may be passed as an argument or
through STDIN.  If only URL is given, it will be downloaded and its
contents used.

Options:
    -h, --heading HEADING     Heading
    -r, --readability         Capture web page article with python-readability
    -t, --template TEMPLATE   org-capture template key (default: w)
    -u, --url URL             URL

    --debug  Print debug info
    --help   I need somebody!
EOF
}

function urlencode {
    python -c "
from __future__ import print_function
try:
    from urllib import quote  # Python 2
except ImportError:
    from urllib.parse import quote  # Python 3
import sys

print(quote(sys.stdin.read()[:-1], safe=''))"
}

# * Args

args=$(getopt -n "$0" -o dh:rt:u: -l debug,help,heading:,readability,template:,url: -- "$@") \
    || die "Unable to parse args.  Is getopt installed?"
eval set -- "$args"

while true
do
    case "$1" in
        -d|--debug)
            debug=true
            debug "Debugging on"
            ;;
        --help)
            usage
            exit
            ;;
        -h|--heading)
            shift
            heading="$1"
            ;;
        -r|--readability)
            protocol="capture-eww-readable"
            readability=true
            ;;
        -t|--template)
            shift
            template="$1"
            ;;
        -u|--url)
            shift
            url="$1"
            ;;
        --)
            # Remaining args
            shift
            rest=("$@")
            break
            ;;
    esac

    shift
done

debug "ARGS: $args"
debug "Remaining args: ${rest[@]}"

# * Main

# ** Get HTML

if [[ -n $@ ]]
then
    debug "HTML from args"

    html="$@"

elif ! [[ -t 0 ]]
then
    debug "HTML from STDIN"

    html=$(cat)

elif [[ -n $url && ! -n $readability ]]
then
    debug "Only URL given; downloading..."

    # Download URL
    html=$(curl "$url") || die "Unable to download $url"

    # Get HTML title for heading
    heading=$(sed -nr '/<title>/{s|.*<title>([^<]+)</title>.*|\1|i;p;q};' <<<"$html") || heading="A web page with no name"

    debug "Using heading: $heading"

elif [[ -n $readability ]]
then
    debug "Using readability"

else
    usage
    echo
    die "I need somethin' ta go on, Cap'n!"
fi

# ** Check URL
# The URL shouldn't be empty

[[ -n $url ]] || url="http://example.com"

# ** URL-encode html

heading=$(urlencode <<<"$heading") || die "Unable to urlencode heading."
url=$(urlencode <<<"$url") || die "Unable to urlencode URL."
html=$(urlencode <<<"$html") || die "Unable to urlencode HTML."

# ** Send to Emacs

emacsclient "org-protocol://$protocol?template=$template&url=$url&title=$heading&body=$html"

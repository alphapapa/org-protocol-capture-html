#!/bin/bash

# * Defaults

heading="Heading"
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
    -t, --template TEMPLATE   org-capture template key (default: w)
    -u, --url URL             URL

    --debug  Print debug info
    --help   I need somebody!
EOF
}

function urlencode {
    python -c "import sys, urllib; print urllib.quote(sys.stdin.read(), safe='')"
}

# * Args

args=$(getopt -n "$0" -o dh:t:u: -l debug,help,heading:,template:,url: -- "$@") || exit 1
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

# ** Check template

if [[ ${#template} -gt 1 ]]
then
    die "Template key should be one letter."
fi

# ** Get HTML

if [[ -n $@ ]]
then
    debug "HTML from args"

    html="$@"

elif ! [[ -t 0 ]]
then
    debug "HTML from STDIN"

    html=$(cat)

elif [[ -n $url ]]
then
    debug "Only URL given; downloading..."

    # Download URL
    html=$(curl "$url") || die "Unable to download $url"

    # Get HTML title for heading
    heading=$(sed -nr '/<title>/{s|.*<title>([^<]+)</title>.*|\1|i;p;q};' <<<"$html") || heading="A web page with no name"

    debug "Using heading: $heading"

else
    usage
    echo
    die "I need somethin' ta go on, Cap'n!"
fi

# ** Check URL
# The URL shouldn't be empty

[[ -n $url ]] || url="http://example.com"

# ** URL-encode html

heading=$(urlencode <<<"$heading")
url=$(urlencode <<<"$url")
html=$(urlencode <<<"$html")

# ** Send to Emacs

emacsclient "org-protocol://capture-html://$template/$url/$heading/$html"

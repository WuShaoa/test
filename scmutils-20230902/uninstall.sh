#!/bin/bash
set -e

get-target-directory ()
{
    mit-scheme --batch-mode --no-init-file <<EOF
(write-string (->namestring (system-library-directory-pathname)))
(newline)
(exit)
EOF
}

declare -r TARGET=$(get-target-directory)
rm -f "${TARGET}"mechanics.com
rm -rf "${TARGET}"scmutils

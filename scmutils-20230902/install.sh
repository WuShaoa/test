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

cd "$(dirname "${0}")"

declare -r TARGET=$(get-target-directory)
declare -ra SRCS=($(find * -type f -name '*.bci'))
for SRC in "${SRCS[@]}"; do
    install -d "${TARGET}"scmutils/"$(dirname "${SRC}")"
    cp -a "${SRC}" "${TARGET}"scmutils/"${SRC}"
done
cp -a mechanics.com "${TARGET}".

#!/bin/bash
export MITSCHEME_HEAP_SIZE=100000
export MITSCHEME_BAND=mechanics.com
exec mit-scheme "${@}"

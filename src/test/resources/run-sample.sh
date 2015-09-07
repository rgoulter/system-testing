#!/bin/bash

##
## Helper script to capture output into a file,
## piping to a format like "CMD.ARG_ARG_ARG.FILE.HOST.VERSION.txt"
## e.g.
##    $ cd /path/to/hg/repo
##    $ /path/to/run-sample.sh ./sleek examples/working/sleek/sleek.slk
##

# Intend to echo A.B_C_..._D.E

# Assume good # args

function filename_for_command {
    # Command
    # (first arg)
    # Use basename, for clarity
    OUTPUT=$(basename ${1})
    shift 1

    # Args
    # (once-and-loop, so we get _ only between the args)
    if [ $# -gt 1 ]
      then
      OUTPUT="${OUTPUT}.${1}"
      shift 1
    fi

    while [ $# -gt 1 ]
      do
      OUTPUT="${OUTPUT}_${1}"
      shift 1
    done

    # Filename
    # (last arg)
    # Use basename, for clarity,
    # and b/c I wouldn't know how to replace '/' with safe..
    OUTPUT="${OUTPUT}.$(basename ${1})"


    # Hostname
    OUTPUT="${OUTPUT}.$(hostname)"

    # Assuming we're running from HG directory
    # HG version
    OUTPUT="${OUTPUT}.$(hg identify)"

    # Friendly extension
    OUTPUT="${OUTPUT}.txt"

    echo $OUTPUT
}

# directory of this bash script
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

FILENAME=$(filename_for_command $*)

$* | tee $DIR/$FILENAME

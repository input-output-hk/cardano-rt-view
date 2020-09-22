#!/bin/sh

set -e

readonly JSON_WITH_LOG_OBJECTS=$1
readonly UNIX_SOCKET=$2

readonly MINIFIED_JSON=/tmp/sender-logObjects-minified.json
readonly PREPARED_FILE=/tmp/sender-hostname-with-logObjects
readonly HOSTNAME="linux"

# Please note that, due to the TraceAcceptor's code, the file that
# will be passed to UNIX-socket must contain exactly 2 lines:
# 1. host name
# 2. [LogObject] encoded to JSON (minified, without newlines!).

# Minify JSON from passed file.
jq -c . < "${JSON_WITH_LOG_OBJECTS}" > "${MINIFIED_JSON}"
# Prepare the file.
echo "${HOSTNAME}" >> "${PREPARED_FILE}"
cat "${MINIFIED_JSON}" >> "${PREPARED_FILE}"
# Send it to passed UNIX socket (drop connection after that).
nc -N -U "${UNIX_SOCKET}" < "${PREPARED_FILE}"

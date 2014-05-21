#!/bin/bash
CRATE_DEFAULT_VERSION="0.39.3"
echo $PWD

if [ "${CRATE_VERSION}x" = "x" ]
then
    echo "CRATE_VERSION not set"
    echo "using default version of ${CRATE_DEFAULT_VERSION}"
    export CRATE_VERSION=${CRATE_DEFAULT_VERSION}
fi

export CRATE_TAR=crate-${CRATE_VERSION}.tar.gz
export TMP_DIR=/tmp/crate

mkdir -p ${TMP_DIR}
if [ ! -f ${TMP_DIR}/crate/bin/crate ]
then
    cd ${TMP_DIR}
    wget -q --no-check-certificate https://cdn.crate.io/downloads/releases/${CRATE_TAR}
    mkdir -p crate
    tar -xz -C crate --strip-components 1 -f ${CRATE_TAR}
    cd -
fi

function cleanup() {
  kill -TERM `cat ${TMP_DIR}/crate.pid`
}

${TMP_DIR}/crate/bin/crate -d -p ${TMP_DIR}/crate.pid || exit 1
chmod u+x $PWD/rebar && $PWD/rebar clean compile && $PWD/rebar skip_deps=true eunit && $PWD/rebar skip_deps=true ct || exit 1
RETVAL=$?

trap cleanup EXIT
exit $RETVAL

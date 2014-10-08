#!/bin/bash
CRATE_DEFAULT_VERSION="0.44.4"

if [ "${CRATE_VERSION}x" = "x" ]
then
    echo "CRATE_VERSION not set"
    echo "using default version of ${CRATE_DEFAULT_VERSION}"
    export CRATE_VERSION=${CRATE_DEFAULT_VERSION}
fi

export CRATE_TAR="crate-${CRATE_VERSION}.tar.gz"
export TMP_DIR="/tmp/crate"
export CRATE_DIR="${TMP_DIR}/crate_${CRATE_VERSION}"

mkdir -p ${TMP_DIR}
if [ ! -f ${CRATE_DIR}/bin/crate ]
then
    echo "downloading crate ${CRATE_VERSION}"
    cd ${TMP_DIR}
    wget -q --no-check-certificate https://cdn.crate.io/downloads/releases/${CRATE_TAR} || exit 1
    mkdir -p ${CRATE_DIR}
    tar -xzf ${CRATE_TAR} -C ${CRATE_DIR} --strip-components 1
    cd -
fi

make clean get-deps compile || exit 1

echo "running eunit..."
make eunit || exit 1

function cleanup() {
  echo "stopping crate."
  kill -TERM `cat ${TMP_DIR}/crate.pid`
}
trap cleanup EXIT

echo "running common test..."
echo "starting crate ..."
${CRATE_DIR}/bin/crate -Des.network.bind_host=127.0.0.1 -d -p ${TMP_DIR}/crate.pid || exit 1
sleep 10
curl -XPOST localhost:4200/_sql -d'{"stmt":"select * from sys.cluster"}' 2&> /dev/null
make ct || exit 1
RETVAL=$?

exit $RETVAL

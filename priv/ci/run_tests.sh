#!/bin/bash
CRATE_DEFAULT_VERSION="0.50.1"

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

./rebar3 as test clean compile || exit 1

function cleanup() {
  echo "stopping crate..."
  kill -TERM `cat ${TMP_DIR}/crate1.pid`
  kill -TERM `cat ${TMP_DIR}/crate2.pid`
  echo "stopped."
  echo "cleaning crate dirs..."
  rm -rf ${CRATE_DIR}/data/*
  echo "cleaned."
}
trap cleanup EXIT

echo "starting crate 1 ..."
${CRATE_DIR}/bin/crate \
    -Des.network.host=127.0.0.1 \
    -Des.discovery.zen.ping.multicast.enabled=false \
    -Des.discovery.zen.ping.unicast.hosts="127.0.0.1:49201" \
    -Des.transport.tcp.port=49200 \
    -Des.http.port=48200 \
    -Des.es.api.enabled=true \
    -d -p ${TMP_DIR}/crate1.pid || exit 1
echo "starting crate 2 ..."
${CRATE_DIR}/bin/crate \
    -Des.network.host=127.0.0.1 \
    -Des.discovery.zen.ping.multicast.enabled=false \
    -Des.discovery.zen.ping.unicast.hosts="127.0.0.1:49200" \
    -Des.transport.tcp.port=49201 \
    -Des.http.port=48201 \
    -Des.es.api.enabled=true \
    -d -p ${TMP_DIR}/crate2.pid || exit 1
sleep 10
curl -XPOST localhost:48200/_sql -d'{"stmt":"select * from sys.cluster"}' 2&> /dev/null
echo "crate 1 started."
echo "crate 2 started."
curl -XPOST localhost:48201/_sql -d'{"stmt":"select * from sys.cluster"}' 2&> /dev/null
./rebar3 as test do eunit, ct, cover || exit 1
RETVAL=$?

exit $RETVAL

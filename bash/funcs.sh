#!/bin/bash

# Commonly used functions

export SFDEVCLUSTER_SETUP_SCRIPT="/opt/microsoft/sdk/servicefabric/common/clustersetup/devclustersetup.sh"
setup_sfdevcluster()
{
    set -x
    sudo $SFDEVCLUSTER_SETUP_SCRIPT
    date
    set +x
}


docker_ip()
{
    if [[ "$#" -ne 1 ]]; then
        echo "Usage: ${FUNCNAME[0]} [CONTAINER_NAME_OR_ID]"
        return 1
    fi
    docker inspect --format '{{range .NetworkSettings.Networks}}{{.IPAddress}} {{end}}' $1
}

create_cert()
{
    if [[ "$#" -ne 2 || $1 = "-h" || $1 = "--help" ]]; then
        echo "Usage: ${FUNCNAME[0]} [CERT_NAME] [SUBJECT]"
        return 1
    fi
    local CERT_DIR="$HOME/.certs"
    local CERT_NAME=$1
    local SUBJECT=$2
    openssl req -x509 -newkey rsa:2048 -keyout "$CERT_DIR/$CERT_NAME.pem" -days 365 \
        -nodes -subj "$SUBJECT" -outform DER -out "$CERT_DIR/$CERT_NAME.cer" && \
        openssl rsa -in "$CERT_DIR/$CERT_NAME.pem" -outform PVK -pvk-strong -out "$CERT_DIR/$CERT_NAME.pvk"
}


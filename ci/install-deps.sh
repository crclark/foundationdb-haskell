#!/bin/bash

set -e

FDB_VER=${1}

sudo apt install python2
wget https://github.com/apple/foundationdb/releases/download/${FDB_VER}/foundationdb-clients_${FDB_VER}-1_amd64.deb
wget https://github.com/apple/foundationdb/releases/download/${FDB_VER}/foundationdb-server_${FDB_VER}-1_amd64.deb
sudo apt install --fix-broken ./foundationdb-clients_${FDB_VER}-1_amd64.deb
# 6.3.x seems to require us to make this directory
sudo mkdir -p /var/lib/foundationdb/data
sudo apt install --fix-broken ./foundationdb-server_${FDB_VER}-1_amd64.deb
sudo apt-get install c2hs

#!/bin/bash

FDB_VER=${1}

sudo apt install python-is-python3
wget https://github.com/apple/foundationdb/releases/download/${FDB_VER}/foundationdb-clients_${FDB_VER}-1_amd64.deb
wget https://github.com/apple/foundationdb/releases/download/${FDB_VER}/foundationdb-server_${FDB_VER}-1_amd64.deb
sudo dpkg -i foundationdb-clients_${FDB_VER}-1_amd64.deb
# 6.3.x seems to require us to make this directory
sudo mkdir -p /var/lib/foundationdb/data
sudo dpkg -i foundationdb-server_${FDB_VER}-1_amd64.deb
sudo apt-get install c2hs

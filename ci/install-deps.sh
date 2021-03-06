#!/bin/bash

FDB_VER=${1}

sudo apt install python
wget https://www.foundationdb.org/downloads/${FDB_VER}/ubuntu/installers/foundationdb-clients_${FDB_VER}-1_amd64.deb
wget https://www.foundationdb.org/downloads/${FDB_VER}/ubuntu/installers/foundationdb-server_${FDB_VER}-1_amd64.deb
sudo dpkg -i foundationdb-clients_${FDB_VER}-1_amd64.deb
# 6.3.x seems to require us to make this directory
sudo mkdir -p /var/lib/foundationdb/data
sudo dpkg -i foundationdb-server_${FDB_VER}-1_amd64.deb
sudo apt-get install c2hs

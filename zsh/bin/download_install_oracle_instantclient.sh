#!/usr/bin/env zsh

INSTANT_CLIENT=instantclient-basic-linux.x64-23.8.0.25.04.zip

wget -c https://download.oracle.com/otn_software/linux/instantclient/2380000/${INSTANT_CLIENT} -P /tmp
mkdir -p ~/.oracle
unzip /tmp/${INSTANT_CLIENT} -d ~/.oracle

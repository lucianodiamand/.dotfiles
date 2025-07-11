#!/bin/bash

INSTANT_CLIENT=instantclient-basic-linux.x64-23.8.0.25.04.zip

wget -c https://download.oracle.com/otn_software/linux/instantclient/2380000/${INSTANT_CLIENT} -P /tmp
sudo mkdir -p /opt/oracle
sudo unzip /tmp/${INSTANT_CLIENT} -d /opt/oracle

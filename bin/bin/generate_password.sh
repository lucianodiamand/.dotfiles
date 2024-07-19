#!/bin/bash
mkpasswd -m sha-512 -S $(pwgen -ns 16 1) $1

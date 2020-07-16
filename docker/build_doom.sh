#!/bin/bash

./build.sh
sudo docker build -t drake_template_doom:4.0.0 -f Dockerfile.doom .

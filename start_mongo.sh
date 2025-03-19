#!/bin/bash

podman pull docker.io/mongodb/mongodb-community-server:latest
podman rm mongodb
podman run --name mongodb -p 27017:27017 -d mongodb/mongodb-community-server:latest

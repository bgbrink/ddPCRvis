#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /srv/log/shiny-server
chown shiny.shiny /srv/log/shiny-server

exec shiny-server 2>&1

#!/bin/sh

echo "starting nginx on port 8002"
nginx -c nginx.conf

echo "starting dynamic service on port 8001"
ruby dynamic.rb &

echo "starting cache service on port 8000"
cd .. && sh ./start-dev.sh 

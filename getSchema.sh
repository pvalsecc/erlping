#!/usr/bin/env bash

set -e

docker exec -ti orientdb bin/console.sh "connect remote:localhost/erlping root root; export schema /tmp/schema"
docker cp orientdb:/tmp/schema.json.gz schema.json.gz
gunzip < schema.json.gz | python -m json.tool > schema.json
rm schema.json.gz

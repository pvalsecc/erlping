docker stop orientdb
docker rm orientdb
docker run -d --name orientdb -p 2424:2424 -p 2480:2480 -v /home/patrick/src/erlang/erlping/db/data:/orientdb/databases -e ORIENTDB_ROOT_PASSWORD=root orientdb:latest

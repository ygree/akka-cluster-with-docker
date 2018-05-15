
Cluster Demo with Docker
========================

This is a Akka demo application and cluster configuration intended to help understand how Akka clustering works in practice.

It uses Docker to simulate cluster nodes and uses iptables to simulate network partitioning. Some other failure simulations can also be achieved by killing/pausing/stopping individual docker containers.

Each aample application logs its cluster status events into a file to the mounted folder `cluster/events`.

1. Build application

```
cd akka-apps
./build
```

2. Run cluster 

```
cd cluster

./up
```

# Cluster topology

cd cluster

# Run cluster nodes

./up config/sbr-keep-majority.conf
./up config/sbr-keep-oldest.conf

# Shutdown cluster nodes

./down

# Emulate partitioning between nodes 1 and 3

./connectivity off 1 3

# Restore connectivity between nodes 1 and 3

./connectivity on 1 3

# Clean up all iptable restrictions for node 4

./clear-rules 4

# Connect to node 5

./console 5

# Emulate network partitioning (1 2) (3 4 5)

./connectivity-12-345 off

# Pause/unpause/restart indiviual node 4

docker-compose pause node-4
docker-compose unpause node-4
docker-compose stop node-4
docker-compose start node-4
docker-compose restart node-4




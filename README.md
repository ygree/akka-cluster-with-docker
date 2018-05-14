

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

./up

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
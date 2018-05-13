
1. Build java-base docker image that include iptables to emulate network issues

```
cd docker-image
./build
```

2. Build application

```
cd akka-apps
./build
```

3. Run cluster 

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

# Connect node 3 to network A

./connect A 3

# Disconnect nodes 3 from network A

./disconnect A 3

# Connect to console of node 1

./console 1


# IPTables turn off all iptables communications

docker-compose exec node-1 sudo iptables -A INPUT -j DROP

docker-compose exec node-1 sudo iptables -D INPUT -j DROP

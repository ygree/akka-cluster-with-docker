
Cluster Demo with Docker
========================

This is a Akka demo application and cluster configuration intended to help understand how Akka clustering works in practice.

It uses Docker to run cluster nodes and iptables to simulate network partitions. Some other failure simulations can also be achieved by killing/pausing/stopping individual docker containers.

Each sample application logs its cluster status events into a file to the mounted folder `cluster/events`.

Also there is a Monitor application that communicates with the Akka cluster and draws the state of the cluster in the browser.

1. Build the Akka sample application

> Note: the Akka sample application uses Lightbed Split-Brain-Resolver that is a commercial feature.
You will need access to the Lightbend Bintray repository in order to build this sample app.

```
cd akka-apps
./build
```

2. Build the Monitor application

> Note: this is Elm application and it requires Elm 0.19

```
cd monitor-app
./build
```

3. Run cluster 

> Note: you will need Docker and Docker Compose installed locally in order to run it.

Once the sample application and the Monitor application are built you can run the cluster with docker-compose.

```
cd cluster

./up <config file>
```
e.g. `./up configs/sbr-keep-majority.conf`

# Open the Monitor application

Open (http://localhost:8558) in the browser

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

# Akka Management HTTP endpoint

`docker-compose exec node-1 curl node-2:8558/cluster/members`

or

`curl localhost:8558/node-2/cluster/members`

> See `https://developer.lightbend.com/docs/akka-management/current/cluster-http-management.html` for more information.

in the latest case `control-node` provides proxy access by Nginx to each node Akka Management HTTP endpoint. See `cluster/control-node` for details.


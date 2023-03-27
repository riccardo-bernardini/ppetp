#!/bin/bash

#**************************************************************************************
#
#
#	name:		nat_traversing_simulation.sh
#	author: 	Roberto Cesco Fabbro
#	date:		Thu Feb 24 2009
#
#	description:	This script simulate the execution of the PPETP software
#			with 2 peers and one server. One of the two peers is
#			behind a NAT. This script verify that that peer is
#			however achievable from the server and the other peer.
#			to simulate the network we use VirtualBox, vith 4 VM
#			registered (2 peers, 1 server and 1 NAT).
#
#
#**************************************************************************************

# SIMULATION CONFIGURATION


# Control addresses of the VMs
LOCAL_ADDR=10.10.10.10

PEER_1_ADDR=10.10.10.1
PEER_NAT_ADDR=10.10.10.2
NAT_ADDR=10.10.10.3
SERVER_ADDR=10.10.10.4

#Logic addresses
PEER_1=192.168.0.1
PEER_NAT=192.168.1.1
SERVER=192.168.0.2

# Working directory on VMs
WD="/home/dsl"

#**************************************************************************************

# START SIMULATION

echo
echo  "PPETP NAT traversing simulation." 
echo



echo "Configure NAT"
ssh $NAT_ADDR "$WD/nat_config.sh"
echo "NAT Configurated"
echo

echo "Starting server"
ssh $SERVER_ADDR "$WD/execute_server.sh" 
echo "Server started"

echo "Starting NATed peer"
ssh $PEER_NAT_ADDR "$WD/execute_client.sh $PEER_NAT $SERVER"
echo "Peer started"

echo "Starting second peer and test NAT traversing"
ssh $PEER_1_ADDR "$WD/execute_client_test.sh $PEER_1 $SERVER"
echo "Second peer started"

echo "Kill all processes"
ssh $SERVER_ADDR "killall server_main & killall client" &> /dev/null &
ssh $PEER_1_ADDR "killall client" &> /dev/null &
ssh $PEER_NAT_ADDR "killall client" &> /dev/null &
echo "All processes Killed"


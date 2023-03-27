#!/bin/bash

#*************************************************************************************
#
#	name:		send_halt_to_all.sh
#
#	author:		Roberto Cesco Fabbro
#
#	date:		Thu Mar 05 2009
#
#	description:	This script send an HALT command to all the VMs, and the one
#			to the machine on which the scritp is been executed 
#			(usually the control machine)
#
#*************************************************************************************


# Control addresses of the VMs
LOCAL_ADDR=10.10.10.10

PEER_1_ADDR=10.10.10.1
PEER_NAT_ADDR=10.10.10.2
NAT_ADDR=10.10.10.3
SERVER_ADDR=10.10.10.4



echo "Send HALT command to VM's"
ssh $SERVER_ADDR halt
ssh $PEER_1_ADDR halt
ssh $PEER_NAT_ADDR halt
ssh $NAT_ADDR halt
echo "All HALT sent"
echo
echo "Halt ME!"
halt

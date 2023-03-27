#!/bin/bash

#*******************************************************************************************************
#
#	name: 		start_virtual_machine.sh
#
#	author:		Roberto Cesco Fabbro
#
#	date:		Thu Mar 05 2009
#
#	description:	This file is only a utility to launch automatically all the Virtual Machine 
#			for the NAT_Traversing test. To verify the exact name of the VMs use the command
#
# 			VBoxManage list vms | grep Name
#
#********************************************************************************************************


# Start the VMs
echo "Start the VMs..." 
#
VBoxManage startvm "control" &> /dev/null &
VBoxManage startvm "peer1" &> /dev/null &
VBoxManage startvm "peer_nat" &> /dev/null &
VBoxManage startvm "server" &> /dev/null &
VBoxManage startvm "NAT" &> /dev/null &
 
echo "VMs Started."



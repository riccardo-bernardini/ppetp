#!/bin/bash

#******************************************************************************
#
#	name:		execute_server.sh
#
#	author:		Roberto Cesco Fabbro
#
#	date:		Fri Feb 27 2009
#
#	description:	This script permit to execute the PPETP server; it 
#			wait that the server's client is been started before
#			to terminate the script
#
#
#******************************************************************************

# CONFIGURATION


# Working directory
WD=/home/dsl

# Temporary file
TMP_FILE=/var/log/PPETP_Test




#******************************************************************************

cd $WD

# Execute the web server

./server_main &> /dev/null &


# Execute the server's client and wait until the client print the string
# "Client: Started" 

./execute_client.sh


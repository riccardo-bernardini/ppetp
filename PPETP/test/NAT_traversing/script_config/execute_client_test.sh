#!/bin/bash

#******************************************************************************
#
#	name:		execute_client.sh
#
#	author:		Roberto Cesco Fabbro
#
#	date:		Fri Feb 27 2009
#
#	description:	This script permit to execute a PPETP client; it 
#			wait that the client is been started.
#			It accept 0 or 2 parameters. 
#			
#			$1 the client address 
#			$2 the server address
#
#******************************************************************************

# CONFIGURATION


# Working directory
WD=/home/dsl

# Temporary file
TMP_FILE=/var/log/PPETP_Test




#******************************************************************************

cd $WD


# Execute the client and wait until the client print the string "Client: Started" 


if [ -e $TMP_FILE ]; then
	rm -f $TMP_FILE
fi

./client $1 $2 &> $TMP_FILE  &

OK="WAIT"

while [ "$OK" = "WAIT" ]; do
	
	OK=$(tail $TMP_FILE | grep "NAT Traversed:")

	if [ -z "$OK" ]; then
		OK="WAIT"
	fi
	
	sleep 1
done

echo $OK



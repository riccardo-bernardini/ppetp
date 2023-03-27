#!/bin/sh -e

cd ../..
export REQUEST_METHOD=POST
program=./p2p.rb

rm /tmp/a.txt || true

echo 'command=create_stream&server=www.yahoo.com&stream=tennis&bandwidth=1.0e6&profile_name=basic' | $program
echo 'command=create_stream&server=www.yahoo.com&stream=formula%201&bandwidth=1.5e6&profile_name=vandermonde&param-vandermonde-gf-size=16&param-vandermonde-redfac=5' | $program
echo 'command=join_user&server=www.yahoo.com&stream=tennis&username=root-server&upload_bw=5e7&address_type=ip&address=127.0.0.1%205267&n_inputs=0&n_channels=20&peer_id=7561291' | $program
echo 'command=join_user&server=www.yahoo.com&stream=formula%201&username=root-server2&upload_bw=5e7&address_type=ip&address=127.0.0.1%203334&n_inputs=0&n_channels=20&peer_id=1617191' | $program
echo 'command=join_user&server=www.yahoo.com&stream=formula%201&username=pippo&upload_bw=650e3&address_type=ip&address=192.0.10.1%206578&n_inputs=6&n_channels=2&peer_id=112710' | $program
echo 'command=join_user&server=www.yahoo.com&stream=formula%201&username=pluto&upload_bw=350e3&address_type=ip&address=192.0.15.21%206676&n_inputs=6&n_channels=2&peer_id=32351' | $program
echo 'command=remove_user&server=www.yahoo.com&stream=formula%201&username=pluto' | $program
echo 'command=destroy_stream&server=www.yahoo.com&stream=formula%201' | $program


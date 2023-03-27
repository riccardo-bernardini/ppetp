                          ========================
                          ** The MERGING Module **
                          ========================
==========
== GOAL ==
==========

The analog and binary processors produce packets of type NETWORK_CRUMB
which represent analog/binary reduced components.  All the
NETWORK_CRUMB relative to the same timestamp are collected in a single
NETWORK packet which will be sent over the Internet.

  - MERGING 
      is the operation of collecting the NETWORK_CRUMB's into a
      NETWORK packet

  - SPLITTING 
      is the inverse of MERGING


===============
== Interface ==
===============

* The action of SPLITTING is carried out by a function which accepts a
  NETWORK packet and returns an array of crumbs

* The action of MERGING is carried out by a "smart queue" with the
  following methods

     - Receive 
         Receive a network crumb.  A special constant crumb is used to
         denote lost crumbs

     - Extract
         Implemented as an entry conditioned by the presence of a
         ready NETWORK packet.



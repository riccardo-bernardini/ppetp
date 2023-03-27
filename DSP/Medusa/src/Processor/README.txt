                    ==========================
                    ** The PROCESSOR Module **
                    ==========================
==========
== GOAL ==
==========

The PROCESSOR module is the "core" of DSP.  Its duty is

   1. Receive the NETWORK_CRUMBS and decode them to get the reduced
      (analog/binary) components

   2. Combine the reduced components to obtain the full components 

   3. Send the full components to the PLAYER via the SYNTHESIS module

   4. Reduce the full components to obtain new reduced components
      which are encoded to obtain new NETWORK_CRUMB's

   5. Send the NETWORK_CRUMB's to the other peers via the MERGING module

===============
== Interface ==
===============

Since there are at least two possible types of PROCESSOR (analog and
binary), most of the work is done inside a generic package.  The
package formal parameters are

  - Reduced_Type
      the Ada type of the reduced components

  - Full_Type
      the Ada type of the full components

  - Reconstructor
      the Ada type associated to the "oxidizer", an object which
      replies to the following methods

    * Receive
        receive a new reduced packet. Signal to the caller if a
        reconstrution is possible.

    * Recover
        recover, if possible, a full component from the received
        reduced components.  

    * Lost
        by calling Lost the PROCESSOR signals that a packet with a
        given timestamp has been declared "lost" and it will not be
        required in the future.  The Reconstructor can use this
        information, for example, to release memory.

The main resource provided by this package are

   - procedure Set_Destinations
        used to set the queues relative to the MERGING and PARSING
        modules 

   - procedure Receive
        receive a new NETWORK_CRUMB

   - procedure Packet_Needed 
        called when the deadline for a packet with a given timestamp
        is close to expiring.  The processor will pass the request to
        the oxidizers to see if the component can be reconstructed,
        even if in an approximate way.

   - procedure Packet_Lost
        called when a packet with a given timestamp is not necessary
        anymore. The PROCESSOR will pass this information to the
        Oxidizer and the smart queues.

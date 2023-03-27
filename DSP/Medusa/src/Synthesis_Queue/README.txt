                      ======================
                      == SYNTHESIS QUEUES ==
                      ======================

What is a Synthesis Queue?
==========================

A synthesis queue is a queue which accepts in input "pieces" of
packets and returns in output whole packets.  A synthesis queue is
implemented as a protected object since it is typically used by two
different tasks. 

In Medusa there are two synthesis queues: 

  1. The queue connected to the network output which receives reduced
     and encoded components (also known as NETWORK CRUMBS) and returns
     whole network packets

  2. The queue conneceted to the player which receives components and
     returns multimedia packets.

Since most of the work done by a synthesis queue (receive packets,
check if a packet can be synthetized, manage lost packets, and so
on...) is independent on the actual format, the two synthesis queue
are implemented via a generic version of the synthesis queue.

The synthesis queue model
=========================

 * A synthesis queue receives packet pieces (called "crumbs") and
   reconstructs "complete" packets

 * The number of crumbs which enter in a complete packet is a 
   constant which can be known only at run-time (that is, such a
   constant is fixed during the initialization phase of the
   queue). Let N_CRUMBS denote the number of crumbs in a complete
   packet. 

 * Each crumb is uniquely identified by a TIMESTAMP (inherited from
   the complete parent) and a CRUMB_INDEX in 1..N_CRUMBS.  

 * The synthesis is demanded to an external function whose access is
   passed to the synthesis queue during the initialization phase.  The
   synthesis procedure will accept an array of crumbs and will return
   (a) the complete packet and (b) a flag set to true if the
   reconstruction was possible and false otherwise.

 * The methods provided by the synthesis queue are

     - An Initialization method used to fix N_CRUMBS and the synthesis
       function.

     - A function to receive a crumb

     - An entry (conditioned by the presence of a complete packet) to
       extract the next complete packet

     - A procedure to declare "lost" a packet/crumb

     - A function which returns an array with the indexes of the
       crumbs missing for a given timestamp

     - A procedure to "force" the reconstruction of a given packet
       (maybe to be removed)
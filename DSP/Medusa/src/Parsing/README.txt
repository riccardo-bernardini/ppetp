                       ========================
                       ** The PARSING Module **
                       ========================
==========
== GOAL ==
==========

Each original multimedia packet needs to be split into its analog and
binary components.  The goal of the PARSING module is to operate such
a partitioning.  In the PARSING module there are two main operations

  - PARSING
      Take each multimedia packet and partition it in binary and
      analog components

  - SYNTHESIS
      Create a multimedia packet from its binary/analog component



===============
== Interface ==
===============

  * The PARSING operation will be implemented as a procedure which
    takes one multimedia packet and returns an array of components

  * The SYNTHESIS operation will be implemented as a "smart queue" as
    for the MERGING operation in the MERGING module 

=============
== Remarks ==
=============

 * The PARSING module clearly depends on the multimedia format
   (obvious) and how the partitioning is done.

 * Each PARSING implementation will have a subdirectory of its own.

 * The simplest PARSING module is the trivial one where PARSING and
   SYNTHESIS are just identity operations and each multimedia packet is
   considered as a single binary component.

================
== Motivation ==
================

This directory contains the code for the task(s) which receive data
from TCP/IP ports and pass to the "core" of the DSP module.

More in detail, the DSP module expect receiving two different types of
data  

  1) Multimedia-related data received over an UDP port from other
     peers 

  2) Internal command such as "SEND" from other modules

Instead of polling each different source, it is more convenient to
assign each source to a different task whose only duty is to read
packets and enqueue them in an "internal queue" implemented as a
protected object.  This approach has the advantage that the DSP "core"
access only one "source", namely the internal protected queue.

=============
== Content ==
=============

This directory contains

   * network_readers.ad?
        The definition of the reader task.  

   * input_data.ad?
        The definition of the data enqueued on the internal queue,
        plus some constructor functions.
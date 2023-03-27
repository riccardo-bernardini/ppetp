===================
== What is this? ==
===================

This is an "RTP-splitter", that is a piece of software that dispatch
RTP packets to different players on the basis of the SSRC field (see
RFC 3550 for details about the SSRC field, here it suffices to say
that it is a field of the RTP header that identifies the source of the
packet).  

More precisely, the RTP-splitter has the following behaviour

  1) Listen on the UDP port specified by the user

  2) When an RTP packet arrives

      2.1) If the SSRC of the packet is "new" (that is, this is the
           first packet with that SSRC)

             2.1.a) Start a new player and ask it to reproduce the
                    multimedia stream that will receive on  a
                    localhost (127.0.0.1) UDP port

      2.2) If the SSRC is not new, send the packet to the
           corresponding player

  3) If after T seconds no more packets with a given SSRC are
     received, close the corresponding player

==========================
== How do I install it? ==
==========================

To be written.  Part of the code is in Ada, part is in Ruby, so you
need a Ruby interpreter (anything after 1.8 should do, no special
frills are required) and an Ada 2005 compiler.  Currently the Ada code
uses GNAT extensions for socket library access, so you should use GNAT.

======================
== How do I use it? ==
======================

Just run

  rtp_splitter.rb [--input-port=port] [--timeout=t] [--player=player]


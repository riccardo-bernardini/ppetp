                       =========================
                       ** The ENCODING Module **
                       =========================
==========
== GOAL ==
==========

In order to be transmitted, each binary/analog component must be
converted from the internal format into "a sequence of bytes" which in
turns need to be converted back to the internal format at the remote
peer.  Such back-and-forth conversion is the duty of the ENCODING
module.

It is clear that the implementative details of the ENCODING module
depend on the component type (analog or binary) and on the technique
used for the encoding.  At least one implementation must be present:
the trivial encoding for binary components.
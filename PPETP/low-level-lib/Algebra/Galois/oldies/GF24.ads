with Ada.Streams, Interfaces;
use  Ada.Streams, Interfaces;

with Gf_2p_Varsize;

package GF24 is
   new Gf_2p_Varsize(Exponent   => 24,
                     Basic_Type => Unsigned_24);


--                              -*- Mode: Ada -*-
--  Filename        : gf8.ads
--  Description     : Specialization to GF(2^8) of GF_2p_Varsize
--  Author          : Finta Tartaruga
--  Created On      : Wed Sep 10 23:01:25 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

with Ada.Streams, Interfaces;
use  Ada.Streams, Interfaces;

with Gf_2p_Varsize;

package GF8 is
   new Gf_2p_Varsize(Exponent   => 8,
                     Basic_Type => Unsigned_8);


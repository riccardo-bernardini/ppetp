--                              -*- Mode: Ada -*-
--  Filename        : gf32.ads
--  Description     : Specialization to GF(2^32) of GF_2p_Varsize
--  Author          : Finta Tartaruga
--  Created On      : Wed Sep 10 23:01:25 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Tested

with Ada.Streams, Interfaces;
use  Ada.Streams, Interfaces;

with Gf_2p_Varsize;

package GF32 is
   new Gf_2p_Varsize(Exponent   => 32,
                     Basic_Type => Unsigned_32);


--                              -*- Mode: Ada -*-
--  Filename        : gf16.ads
--  Description     : Specialization to GF(2^16) of GF_2p_Varsize
--  Author          : Finta Tartaruga
--  Created On      : Wed Sep 10 23:01:25 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>
--  Type            : <instance(gf_2p_varsize)>

with Ada.Streams, Interfaces;
use  Ada.Streams, Interfaces;

with Gf_2p_Varsize;

package GF16 is
   new Gf_2p_Varsize(Exponent   => 16,
                     Basic_Type => Unsigned_16);


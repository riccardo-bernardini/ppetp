--                              -*- Mode: Ada -*-
--  Filename        : random_integer.ads
--  Description     : Random Integer Number Generator
--  Author          : Roberto Cesco Fabbro
--  Created On      : Gen, 27 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>
--  Type            : <instance>

-- This Package provide define  the generic package Ada.Numerics.Discrete_Random
-- for Integer type

with Ada.Numerics.Discrete_Random;

package Random_Integer is new Ada.Numerics.Discrete_Random (Integer);

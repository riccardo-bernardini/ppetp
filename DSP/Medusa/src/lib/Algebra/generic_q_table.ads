--                              -*- Mode: Ada -*-
--  Filename        : generic_q_table.ads
--  Description     : Quantization table
--  Author          : Finta Tartaruga
--  Created On      : Wed Sep  5 14:29:39 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Basic test OK.

--
-- This generic package implements a quantization table (the only formal
-- parameter of this package is the floating type).
--
-- In a typical lossy coder the input data, after suitable
-- transformations, are quantized.  In order to allow the decoder
-- to reconstruct the signal, it is necessary to include the quantization
-- step in the bitstream.  A simple solution is to choose off-line a
-- set of quantization values indexed by an integer index which is inserted
-- in the bitstream.  This generic package provides functions to help
-- working with quantization tables.
--
-- Formally, a quantization table with N levels is a strictly increasing
-- map Q : {0, 1, ..., N-1} -> Coefficient_Type.
--
-- The main functions exported by this package are
--
generic
   type Coefficient_Type is digits <>;
package Generic_Q_Table is
   type Q_Table_Type is private;

   --==========================--
   -- INITIALIZATION FUNCTIONS --
   --==========================--

   --======================--
   -- High level functions --
   --======================--

   --
   -- Initialize the table with N_levels quantization level
   -- exponentially spaced, i.e., there is a value rho such that
   -- for every n and m
   --
   --               Q(n) = rho**(n-m) * Q(m)
   --
   -- where Q(n) is the value associated to the n-th level.  The
   -- value of rho is computed by the function in order to have the
   -- minimum value Q(0) equal to Q_Min and the maximum value
   -- Q(N_Levels-1) equal to Q_Max.
   --
   procedure Fill_Table (Table    : in out Q_Table_Type;
                         N_Levels : in     Positive;
                         Q_Max    : in     Coefficient_Type;
                         Q_Min    : in     Coefficient_Type);

   --======================--
   -- Low level functions --
   --======================--

   -- Reserve space for Size levels in the table
   procedure Resize (Table : in out Q_Table_Type;
                     Size  : in     Positive);

   -- Force the quantization step of index Idx to value Value.
   -- Idx must be non negative and smaller than the Size value
   -- passed to Resize.
   procedure Set_Q_Step (Table : in out Q_Table_Type;
                         Idx   : in     Integer;
                         Value : in     Coefficient_Type);


   --===================--
   -- READING THE TABLE --
   --===================--

   -- Search in the table for the quantization level closest to
   -- Required.  The quantization step is returned in Obtained and
   -- its index in Index.
   procedure Best_Q_Step (Table    : in     Q_Table_Type;
                          Required : in     Coefficient_Type;
                          Obtained :    out Coefficient_Type;
                          Index    :    out Integer);

   -- Get the quantization step associated with Idx
   function Q_Step (Table : Q_Table_Type;
                    Idx   : Integer)
                   return Coefficient_Type;

private
   type Coefficient_Array    is array(Integer range <>) of Coefficient_Type;
   type Coefficient_Array_Pt is access Coefficient_Array;

   type Q_Table_Type is new Coefficient_Array_Pt;
end Generic_Q_Table;

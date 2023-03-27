--                              -*- Mode: Ada -*-
--  Filename        : id_counters.ads
--  Description     : ID counter
--  Author          : Finta Tartaruga
--  Created On      : Mon Nov 19 18:33:32 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : beta, Basic tests OK

--
-- 1. Goal
-- This package implements an N-dimensional ID counter.
--
-- 2. Motivation
-- An N-dimensional ID counter is an "object" which generates
-- the whole set of N dimensional arrays of Naturals "skinwise", that
-- is, first generates the all-zero vector, then the vectors which have
-- as maximum value, the the vector whose maximum value is two and
-- so on...  More formally, if n is an N-dimensional vector of Naturals
-- and
--
--          ||n|| = max_k |a_n|
--
-- denotes the inf-norm and #(n) denote the position of n in the sequence
-- of generated vectors, the following condition always holds
--
--          ||n|| < ||m||   ==> #(n) < #(m)
--
-- 3. Usage
--
--   C : Counter := New_Counter(3);
--   A : Id;
--
--   A := Current_Id(C);             -- returns A = (0,0,0)
--
--   Next(C); A := Current_Id(C);    -- returns A = (1,0,0)
--   Next(C); A := Current_Id(C);    -- returns A = (1,1,0)
--   Next(C); A := Current_Id(C);    -- returns A = (1,0,1)
--   Next(C); A := Current_Id(C);    -- returns A = (1,1,1)
--   Next(C); A := Current_Id(C);    -- returns A = (0,1,0)
--   Next(C); A := Current_Id(C);    -- returns A = (0,1,1)
--   Next(C); A := Current_Id(C);    -- returns A = (0,0,1)
--   Next(C); A := Current_Id(C);    -- returns A = (0,1,0)
--
--   Next(C); A := Current_Id(C);    -- returns A = (2,0,0)
--   Next(C); A := Current_Id(C);    -- returns A = (2,1,0)
--   Next(C); A := Current_Id(C);    -- returns A = (2,2,0)
--   Next(C); A := Current_Id(C);    -- returns A = (2,0,1)
--
--
package Id_Counters is
   -- Type of an ID counter
   type Counter is private;

   -- An ID is just an array of Natural
   type Id is array(Positive range <>) of Natural;
   type Access_Id is access Id;

   -- Create a new Dim-dimensional ID Counter
   function New_Counter(Dim : Positive)
                        return Counter;

   -- Advance the counter
   procedure Next(C : in out Counter);

   -- Return the current ID stored in the counter
   function Current_Id(C : Counter)
                       return Id;
private
   type Counter is record
      Current_Top  : Natural;
      Top_Position : Positive;
      Id_Buffer    : Access_Id;
   end record;
end Id_Counters;

--                              -*- Mode: Ada -*-
--  Filename        : generic_oxidizer_maps.ads
--  Description     : Cache for reconstruction matrices
--  Author          : Finta Tartaruga
--  Created On      : Tue Aug 21 18:51:01 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Basic testing ok.

--
-- Both types of oxidizers (analogic and binary) reconstructs the
-- original data by left multiplying the received reduced data by
-- the inverse/pseudo-inverse of the matrix of reduction vectors.
-- Since inverse computation can be expensive and it is reasonable
-- to expect that in a typical session only few matrices will be
-- used, storing the computed matrices in a "cache" could
-- improve the overall efficiency.
--
-- This package is a generic package which provides a
-- matrix cache.  Since each reduction vector is uniquely determined
-- by its "index" (a relatively small integer), this package supposes
-- that the Key to the table is an array of "indexes" (formal types
-- Index and Index_Array).
--
-- This package exports two functions: Insert and Get
--

with Ada.Containers.Ordered_Maps;

generic
   type Element is private;  -- type of the matrix to be stored
   type Index   is (<>);     -- type of the index used for r.v.
   type Index_array is array (Positive range <>) of Index;
package Generic_Matrix_Cache is
   subtype Key_Type is Index_Array;
   type Key_Type_Ref is access Key_Type;

   type Table_Key is record
      K : Key_Type_Ref;
   end record;

   function "<" (X, Y : Table_Key) return Boolean;

   package Ox_Maps is
      new Ada.Containers.Ordered_Maps (Key_Type => Table_key,
                                       Element_Type => Element);

   use type Ox_Maps.Map;
   subtype Matrix_Map is Ox_Maps.Map;

   use type Ox_Maps.Cursor;
   subtype Matrix_Map_Cursor is Ox_Maps.Cursor;

   procedure Insert(Map      : in out Matrix_Map;
                    New_Item : in     Element;
                    Key      : in     Index_array);

   procedure Get (Map    : in     Matrix_Map;
                  Key    : in     Index_array;
                  Found  :    out Boolean;
                  Matrix :    out Element);
end Generic_Matrix_Cache;

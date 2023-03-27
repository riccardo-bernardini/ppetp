--                              -*- Mode: Ada -*-
--  Filename        : length_delimited_strings.ads
--  Description     : Parse length delimited strings
--  Author          : Finta Tartaruga
--  Created On      : Mon Feb 25 21:17:17 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides functions to parse length delimited strings
-- (LDS).  The LDS format is a nice format which allows to code any
-- string without caring about special characters, quotes and so on.
-- It is not really suited for human use, but it is just fine for
-- communications between processes.
--
-- A string of L characters is described as follows
--
--   1. The value of L, expressed as a base 10 number (in ASCII coding).
--
--   2. a colon ':'
--
--   3. The L characters of the string
--
-- Any trailing part which does not correspond to a number followed
-- by ':' is ignored.
--
-- For example, the LDS decoding of  "junk & rumenta 123 7:fii foo fuu"
-- gives "fii foo".  Note how the beginning and the end of the string are
-- ignored.
--
-- The functions provided by this package extracts from a given string
-- all the LDS included in it.  For example, from
--
--   "3:foo9:zorro12:! this is ign77ored 10:james bond junk, junk..."
--
-- this package would extract the strings
--
--    "foo"
--    "zorro12:!"
--    "james bond"
--
-- The result is stored in a variable of type LDS_Set_Type which can
-- be interrogated  to get the decoded strings.
--
-- Typical usage
--
--    Parsed : LDS_Set_Type;
--    Size   : Natural;
--
--    Load(Parsed, "3:foo9:zorro12:! 10:james bond ");
--
--    Size := N_Elem(Parsed);        -- returns 3
--    put_line(Element(Parsed, 2));  -- prints "zorro12!"
--    put_line(Element(Parsed, 4));  -- raises Constraint_Error
--
--    Load(Parsed, "4:foo");         -- raises Malformed_LDS
--
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package Length_Delimited_Strings is
   type LDS_Set_Type is tagged private;

   --
   -- Parse the string Source and store the result in LDS_Set.
   -- Raise Malformed_LDS if Source is not a valid LDS.
   --
   procedure Load (LDS_Set : in out LDS_Set_Type;
                   Source  : in     String);

   -- Return the # of strings in LDS_Set
   function N_Elem (LDS_Set : in LDS_Set_Type) return Natural;

   -- Return the Where-th string
   function Element (LDS_Set : in LDS_Set_Type;
                     Where   : in Positive) return String;

   -- Encode What in LDS format
   function Encode(What : String) return String;

   Malformed_LDS : exception;
private
   type Unbounded_Str_Array is array(Positive range <>) of Unbounded_String;
   type Unbounded_Str_Access is access Unbounded_Str_Array;

   type LDS_Set_Type is tagged record
      Set  : Unbounded_Str_Access;
      Size : Natural;
   end record;
end Length_Delimited_Strings;

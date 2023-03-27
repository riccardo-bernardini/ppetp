--                              -*- Mode: Ada -*-
--  Filename        : crypt-sha1.ads
--  Description     : SHA1 digest function
--  Author          : Riccardo Bernardini
--  Created On      : Wed Oct  8 09:24:14 2008
--  Last Modified By: .
--  Last Modified On: Wed Nov  5 17:06:20 2008
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- ===================
-- == What is this? ==
-- ===================
--
-- This package implements the SHA1 message digest function
-- described in FIPS 180-2 (http://csrc.nist.gov/publications/).
-- More precisely, the package provides function Sha1 which
-- accepts the message as an array of Unsigned_8 and returns an
-- array of Unsigned_8 with 20 entries.  For convenience,
-- a version of SHA1 which accepts a String as input parameter
-- is also provided.
--
-- =======================
-- == How do I use it ? ==
-- =======================
--
--     with Crypt.SHA1;     use Crypt.SHA1;
--
--     hash : SHA1_Hash;
--     hash := SHA1("The quick brown fox");
--
with Interfaces;
with Ada.Strings.Unbounded;

use Interfaces;
use Ada.Strings.Unbounded;

package Crypt.SHA1 is
   Block_Size : constant Positive := 64;

   type Byte_Array is array (Integer range <>) of Unsigned_8;

   subtype Sha1_Index   is Integer range 1 .. 20;
   subtype Sha256_Index is Integer range 1 .. 32;
   subtype Sha1_Hash  is Byte_Array (Sha1_Index);

   type Sha256_Has is new Byte_Array (Sha256_Index);

   function Sha1 (Msg : Byte_Array) return Sha1_Hash;
   function Sha1 (Msg : String) return Sha1_Hash;
end Crypt.SHA1;

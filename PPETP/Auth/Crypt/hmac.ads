--                              -*- Mode: Ada -*-
--  Filename        : hmac.ads
--  Description     : Generic implementing the HMAC procedure
--  Author          :
--  Created On      : Tue Oct  7 11:21:47 2008
--  Last Modified By: .
--  Last Modified On: Wed Nov  5 17:07:45 2008
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- ==========================
-- == What this package is ==
-- ==========================
--
-- This generic package provides types and functions which
-- implement the HMAC  (Keyed-Hash Message Authentication
-- Code) procedure, as described in FIPS PUB 198.
-- (http://csrc.nist.gov/publications/).
--
-- HMAC is a general procedure which allows to use any
-- hash function (such as MD5, SHA1, ...) for authentication
-- purposes. Because of the generality of the HMAC, we decided to
-- implement it as a generic package.
--
-- =============================
-- == What this package needs ==
-- =============================
--
-- This package supposes that the underling hash function
-- accepts array of modular types (e.g. Interfaces.Unsigned_8 or
-- Ada.Streams.Stream_Element) and returns digests as fixed
-- length array.
--
-- In order to complete the instantiation of this package, the
-- user must provide
--
--   * The modular type Data_Type
--
--   * The type of a message   (as an array of Data_Type)
--
--   * The type of the digest  (still an array of Data_Type).  Note
--     that it is necessary to give also the type used for the
--     array index.
--
--   * The underling hash function
--
-- ================================
-- == What this package provides ==
-- ================================
--
-- This package provides
--
--   1) a function Hash which takes as argument the key and
--      the message to be hashed and returns the hash
--
--   2) a tagged type Hasher with a method Hash. A Hasher
--      Object must be created with the function New_Hasher
--      which initializes the Hasher with the key to be used.
--
-- The function version can be more convenient for one-time
-- hashing, when a key is used only once.  If a key is
-- going to be used for several hashing, the Hasher version
-- can be slightly more efficient since the key prepocessing
-- is done only once.
--

generic
   -- Type of a single element of a message
   type Data_Type  is mod <>;

   type Msg_Type   is array (Integer range <>) of Data_Type;

   Block_Size : Positive;
   type Digest_Index  is range <>;
   type Digest_Output is array (Digest_Index) of Data_Type;

   with function "xor"(A, B : Data_Type)
                      return Data_Type is <>;

   with function Basic_Hash(X : Msg_Type)
                           return Digest_Output;
package HMAC is
   Output_Length : constant Integer := Digest_Output'Length;

   -- Process message Msg with HMAC and key Key
   function Hash(Key : Msg_Type;
                 Msg : Msg_Type)
                return Digest_Output;

   type Hasher is tagged private;

   -- Create a new Hasher object, initializing it with the
   -- key to be used.
   function New_Hasher (Key : Msg_Type) return Hasher;

   function Hash(H   : Hasher;
                 Msg : Msg_Type) return Digest_Output;
private
   -- Input block for the Basic_Hash function
   subtype Block_Type is Msg_Type (1 .. Block_Size);

   type Hasher is tagged
      record
         Key_I : Block_Type;
         Key_O : Block_Type;
      end record;

end HMAC;

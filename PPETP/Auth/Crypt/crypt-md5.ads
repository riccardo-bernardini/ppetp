--
-- "Glue" package to use the GNAT MD5 library with generic
-- package HMAC.
--

with Interfaces;  use  Interfaces;

package Crypt.MD5 is
   type Index_Type is new Integer range 1..128/8;
   type Msg_Type   is array (Integer range <>) of Unsigned_8;
   type Hash_Type  is array (Index_Type) of Unsigned_8;

   Block_Size : constant Positive := 64;

   function Digest (Msg : Msg_Type)
                   return Hash_Type;
end  Crypt.MD5;

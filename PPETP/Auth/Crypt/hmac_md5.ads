--
-- Example of instantiation of generic package HMAC
--

with HMAC;
with Crypt.MD5;
with Interfaces;    use Interfaces;

package Hmac_Md5 is
new HMAC (Data_Type     => Unsigned_8,
          Msg_Type      => Crypt.MD5.Msg_Type,
          Block_Size    => Crypt.MD5.Block_Size,
          Digest_Index  => Crypt.MD5.Index_Type,
          Digest_Output => Crypt.MD5.Hash_Type,
          Basic_Hash    => Crypt.MD5.Digest);

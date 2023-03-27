--
-- *************************************
-- *** Work in progress! Do not use! ***
-- *************************************
--

package Crypt.Blowfish is
   type Quad_Word is mod 2 ** 32;

   type Data_Block is array (1 .. 2) of Quad_Word;

   type Key_Sizes  is range 1 .. 14;
   type Key_Buffer is array (Key_Sizes range <>) of Quad_Word;

   type Cipher(<>) is limited private;

   function New_Cipher (Key : Key_Buffer)
                        return Cipher;

   function Encrypt (Processor  : Cipher;
                     Clear_Text : Data_Block)
                     return Data_Block;

   function Decrypt (Processor  : Cipher;
                     Cipher_Text : Data_Block)
                     return Data_Block;
private
   type Subkey_Range is range 1 .. 18;
   type Subkey_Buffer is array (Subkey_Range) of Quad_Word;

   type S_Box_Range is range 1 .. 4;
   type Byte is mod 256;
   type S_Box_Data is array (S_Box_Range, Byte) of Quad_Word;

   type Cipher is
      record
         P_Subkey    : Subkey_Buffer;
         S_Box       : S_Box_Data;
      end record;

end Crypt.Blowfish;

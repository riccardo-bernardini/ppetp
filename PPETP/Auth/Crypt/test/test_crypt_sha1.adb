--  Filename: test_crypt_sha.adb
--
--  This file provides a little suite of tests for improving the package
--  Crypt.SHA1 defined in crypt-sha1.ads. These tests are developed according
--  to the description of "Secure Hash Standard" of FIPS 180-2.

with Test_Report , Crypt.SHA1 , Interfaces , Ada.Strings.Unbounded;

use Test_Report , Crypt.SHA1 , Interfaces , Ada.Strings.Unbounded;


procedure Test_Crypt_SHA1 is

   Reporter : Reporter_Type;

   Msg1 : Unbounded_String := To_Unbounded_String("abc");
   Msg2 : Unbounded_String := To_Unbounded_String("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");

   Result1 : SHA1_Hash := (1 => 16#A9#, 2 => 16#99#, 3 => 16#3E#, 4 => 16#36#,
                           5 => 16#47#, 6 =>  16#6#, 7 => 16#81#, 8 => 16#6A#,
                           9 => 16#BA#, 10=> 16#3E#, 11=> 16#25#, 12=> 16#71#,
                           13=> 16#78#, 14=> 16#50#, 15=> 16#C2#, 16=> 16#6C#,
                           17=> 16#9C#, 18=> 16#D0#, 19=> 16#D8#, 20=> 16#9D#);

   Result2 : SHA1_Hash := (1 => 16#84#, 2 => 16#98#, 3 => 16#3E#, 4 => 16#44#,
                           5 => 16#1C#, 6 => 16#3B#, 7 => 16#D2#, 8 => 16#6E#,
                           9 => 16#BA#, 10=> 16#AE#, 11=> 16#4A#, 12=> 16#A1#,
                           13=> 16#F9#, 14=> 16#51#, 15=> 16#29#, 16=> 16#E5#,
                           17=> 16#E5#, 18=> 16#46#, 19=> 16#70#, 20=> 16#F1#);


   type SHA1_Case is
      record
         Msg :  Unbounded_String;
         Hash : SHA1_Hash;
      end record;


   type SHA1_Case_Array is
     array(Positive range <>) of SHA1_Case;

   Cases : SHA1_Case_Array := ((Msg => Msg1 , Hash => Result1),
                               (Msg => Msg2 , Hash => Result2));


   function Check_SHA1 (X : SHA1_Case) return Boolean is
   begin
      return Sha1(To_String(X.Msg)) = X.Hash;
   end Check_Sha1;


   procedure Test_Function_SHA1 is
     new Do_Suite (Test_Case => SHA1_Case,
                   Test_Case_Array => SHA1_Case_Array,
                   Check => Check_SHA1);


begin

   Test_Function_SHA1(Reporter , Cases);

   Final(Reporter);

end Test_Crypt_SHA1;



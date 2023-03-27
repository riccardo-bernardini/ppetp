--  Filename: test_hmac_with_sha1.adb
--
--  This file provides a suite of tests for improving the generic package
--  HMAC defined in hmac.ads. These tests are developed according
--  to the description of "The Keyed-Hash Message Authentication Code (HMAC)"
--  of FIPS 198 using hash function SHA1, as defined in FIPS 190-2, available in
--  this project under the name of Crypt.SHA1 (see crypt-sha1.ads).

with Test_Report , Crypt.SHA1 , Interfaces;
with HMAC;

use Test_Report , Crypt.SHA1 , Interfaces;


procedure Test_HMAC_With_SHA1 is

   type Key_32_bit is array(Positive range <>) of Unsigned_32;


   function To_Byte_Array (A : in Key_32_bit) return Byte_Array is

      J : Integer :=1;
      C : Key_32_bit := A;
   begin

      declare
         B : Byte_Array(1 .. C'Length*4);
         begin
            for I in C'Range
            loop
            B(J)   := Unsigned_8(C(I) / Unsigned_32( 2**24 ));
            C(I)   := C(I) mod Unsigned_32(2 ** 24);

            B(J+1) := Unsigned_8(C(I) / Unsigned_32(2 ** 16));
            C(I)   := C(I) mod Unsigned_32(2 ** 16);

            B(J+2) := Unsigned_8(C(I) / Unsigned_32(2 ** 8));
            C(I)   := C(I) mod Unsigned_32(2 ** 8);

            B(J+3) := Unsigned_8(C(I));

            J := J+4;
         end loop;

         return B;

      end;


   end To_Byte_Array;


   package Test_Hmac is new HMAC(Data_Type => Unsigned_8,
                               Msg_Type => Byte_Array,
                               Block_Size => 64,
                               Digest_Output => Sha1_Hash,
                               Digest_Index => Sha1_Index,
                               Basic_Hash => Sha1 );

   Reporter : Reporter_Type;


   Msg1 : Byte_Array := (1 => 16#53#, 2 => 16#61#, 3 => 16#6d#, 4 => 16#70#,
                         5 => 16#6c#, 6 =>  16#65#, 7 => 16#20#, 8 => 16#23#,
                         9 => 16#31#);

   Msg2 : Byte_Array := (1 => 16#53#, 2 => 16#61#, 3 => 16#6d#, 4 => 16#70#,
                         5 => 16#6c#, 6 =>  16#65#, 7 => 16#20#, 8 => 16#23#,
                         9 => 16#33#);



   Key1 : Key_32_Bit :=  (1 => 16#00010203#, 2 => 16#04050607#,
                          3 => 16#08090a0b#, 4 => 16#0c0d0e0f#,
                          5 => 16#10111213#, 6 => 16#14151617#,
                          7 => 16#18191a1b#, 8 => 16#1c1d1e1f#,
                          9 => 16#20212223#, 10=> 16#24252627#,
                          11=> 16#28292a2b#, 12=> 16#2c2d2e2f#,
                          13=> 16#30313233#, 14=> 16#34353637#,
                          15=> 16#38393a3b#, 16=> 16#3c3d3e3f#);


   Key2 : Key_32_Bit := (1 => 16#50515253#, 2 => 16#54555657#,
                          3 => 16#58595a5b#, 4 => 16#5c5d5e5f#,
                          5 => 16#60616263#, 6 => 16#64656667#,
                          7 => 16#68696a6b#, 8 => 16#6c6d6e6f#,
                          9 => 16#70717273#, 10=> 16#74757677#,
                          11=> 16#78797a7b#, 12=> 16#7c7d7e7f#,
                          13=> 16#80818283#, 14=> 16#84858687#,
                          15=> 16#88898a8b#, 16=> 16#8c8d8e8f#,
                          17=> 16#90919293#, 18=> 16#94959697#,
                          19=> 16#98999a9b#, 20=> 16#9c9d9e9f#,
                          21=> 16#a0a1a2a3#, 22=> 16#a4a5a6a7#,
                          23=> 16#a8a9aaab#, 24=> 16#acadaeaf#,
                          25=> 16#b0b1b2b3#);



   Result1 : SHA1_Hash := (1 => 16#4f#, 2 => 16#4c#, 3 => 16#a3#, 4 => 16#d5#,
                           5 => 16#d6#, 6 => 16#8b#, 7 => 16#a7#, 8 => 16#cc#,
                           9 => 16#0a#, 10=> 16#12#, 11=> 16#08#, 12=> 16#c9#,
                           13=> 16#c6#, 14=> 16#1e#, 15=> 16#9c#, 16=> 16#5d#,
                           17=> 16#a0#, 18=> 16#40#, 19=> 16#3c#, 20=> 16#0a#);

   Result2 : SHA1_Hash := (1 => 16#bc#, 2 => 16#f4#, 3 => 16#1e#, 4 => 16#ab#,
                           5 => 16#8b#, 6 => 16#b2#, 7 => 16#d8#, 8 => 16#02#,
                           9 => 16#f3#, 10=> 16#d0#, 11=> 16#5c#, 12=> 16#af#,
                           13=> 16#7c#, 14=> 16#b0#, 15=> 16#92#, 16=> 16#ec#,
                           17=> 16#f8#, 18=> 16#d1#, 19=> 16#a3#, 20=> 16#aa#);


begin
   New_Suite(Reporter);

   -- Test of function Hash in package HMAC when it gets for input
   -- the Byte_Array "Key" and the Byte_Array "Msg".

   if Test_Hmac.Hash(To_Byte_Array(Key1) , Msg1) = Result1 then
      Success(Reporter);
   else
      Failure(Reporter);
   end if;

   if Test_Hmac.Hash(To_Byte_Array(Key2) , Msg2) = Result2 then
      Success(Reporter);
   else
      Failure(Reporter);
   end if;



   -- Test of function Hash in package HMAC when it gets for input
   -- the Byte_Array "Msg" and the Hasher produced by New_Hasher
   -- wich input is the Byte_Array "Key".

   if Test_Hmac.Hash(Test_Hmac.New_Hasher(To_Byte_Array(Key1)) , Msg1) = Result1 then
      Success(Reporter);
   else
      Failure(Reporter);
   end if;

      if Test_Hmac.Hash(Test_Hmac.New_Hasher(To_Byte_Array(Key2)) , Msg2) = Result2 then
      Success(Reporter);
   else
      Failure(Reporter);
   end if;


   Final(Reporter);

end Test_HMAC_With_SHA1;



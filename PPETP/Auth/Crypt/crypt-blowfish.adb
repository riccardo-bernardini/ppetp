with Ada.Unchecked_Conversion;
private with Crypt.Blowfish.Init_Data;

package body Crypt.Blowfish is
--     type Word_Pair is array (1 .. 2) of Quad_Word;
--
--     function To_Word_Pair is
--       new Ada.Unchecked_Conversion (Source => Data_Block,
--                                     Target => Word_Pair);
--
--     function To_Data_Block is
--       new Ada.Unchecked_Conversion (Source => Word_Pair,
--                                     Target => Data_Block);

   function Feistel_Function (Processor : Cipher;
                              Input     : Quad_Word)
                              return Quad_Word is
      Accum  : Quad_Word;
      Buffer : Quad_Word := Input;
   begin
      Accum := Processor.S_Box (1, Byte(Buffer and 255));
      Buffer := Buffer / 256;

      Accum := Accum + Processor.S_Box (2, Byte(Buffer and 255));
      Buffer := Buffer / 256;

      Accum := Accum xor Processor.S_Box (3, Byte(Buffer and 255));
      Buffer := Buffer / 256;

      Accum := Accum + Processor.S_Box (4, Byte(Buffer));

      return Accum;
   end Feistel_Function;
   pragma Inline (Feistel_Function);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key
     (Processor : in out Cipher;
      Key       : in Key_Buffer)
   is
   --        function Replicate_Key (Key : Key_Buffer)
   --                                return Key_Buffer
   --        is
   --           Result : Key_Buffer (Key_Sizes'Range);
   --           J      : Key_Sizes;
   --        begin
   --           Extend_By_Periodicity :
   --           for I in Result'Range loop
   --              J := Key'First + (I - Result'First) mod Key'Length;
   --              Result (I) := Key (J);
   --           end loop Extend_By_Periodicity;
   --
   --
   --           return Result;
   --        end Replicate_Key;

      J      : Key_Sizes;
      Status : Data_Block;
   begin
      Processor.P_Subkey := Init_Data.Initial_Subkey;
      Processor.S_Box    := Init_Data.Initial_S_Boxes;

      for I in Processor.P_Subkey'Range loop
         J := 1 + Key_Sizes ((I - Processor.P_Subkey'First) mod 14);
         Processor.P_Subkey (I) :=
           Processor.P_Subkey(I) xor Key(J);
      end loop;

      Status := (0, 0);
      declare
         Subkey_Idx : Subkey_Range := Processor.P_Subkey'First;
      begin
         Fill_P_Subkeys :
         while Subkey_Idx <= Processor.P_Subkey'Last loop
            Status := Encrypt (Processor, Status);
            Processor.P_Subkey (Subkey_Idx) := Status (1);
            Processor.P_Subkey (Subkey_Idx + 1) := Status (2);
            Subkey_Idx := Subkey_Idx + 2;
         end loop Fill_P_Subkeys;
      end;

      declare
         Entry_Idx : Byte;
      begin
         Fill_S_Boxes :
         for S_Box_Idx in Processor.S_Box'Range(1) loop
            Entry_Idx := Processor.S_Box'First (2);
            while Entry_Idx <= Processor.S_Box'Last (2) loop
               Status := Encrypt (Processor, Status);
               Processor.S_Box (S_Box_Idx, Entry_Idx) := Status (1);
               Processor.S_Box (S_Box_Idx, Entry_Idx + 1) := Status (2);
               Entry_Idx := Entry_Idx + 2;
            end loop;
         end loop Fill_S_Boxes;
      end;

      raise Program_Error;
   end Set_Key;

   -------------
   -- Encrypt --
   -------------

   function Encrypt
     (Processor  : Cipher;
      Clear_Text : Data_Block)
      return Data_Block
   is
      Swap_Buf : Quad_Word;
      Status   : Data_Block := Clear_Text;
   begin
      for I in Processor.P_Subkey'Range loop
         Status (1) := Status (1) xor Processor.P_Subkey (I);
         Status (2) := Status (2) xor
           Feistel_Function (Processor, Status(1));

         if (I < Processor.P_Subkey'Last) then
            Swap_Buf   := Status (1);
            Status (1) := Status (2);
            Status (2) := Swap_Buf;
         end if;
      end loop;

      Status (1) := Status (1) xor Processor.P_Subkey (18);
      Status (2) := Status (2) xor Processor.P_Subkey (17);

      return Status;
   end Encrypt;

   -------------
   -- Decrypt --
   -------------

   function Decrypt
     (Processor  : Cipher;
      Cipher_Text : Data_Block)
      return Data_Block
   is      Swap_Buf : Quad_Word;
      Status   : Data_Block := Cipher_Text;
   begin

      for I in reverse Subkey_Range(3) .. Subkey_Range(18) loop
         Status (1) := Status (1) xor Processor.P_Subkey (I);
         Status (2) := Status (2) xor
           Feistel_Function (Processor, Status(1));

         if (I /= 3) then
            Swap_Buf   := Status (1);
            Status (1) := Status (2);
            Status (2) := Swap_Buf;
         end if;
      end loop;

      Status (1) := Status (1) xor Processor.P_Subkey (1);
      Status (2) := Status (2) xor Processor.P_Subkey (2);

      return Status;
   end Decrypt;

   ----------------
   -- New_Cipher --
   ----------------

   function New_Cipher
     (Key : Key_Buffer)
      return Cipher
   is
   begin
      return Result : Cipher do
         Set_Key (Result, Key);
      end return;
   end New_Cipher;

end Crypt.Blowfish;

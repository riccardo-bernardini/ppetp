with Interfaces;
use  Interfaces;



with Ada.Unchecked_Conversion;

package body Crypt.SHA1 is
   type Hash_Buffer is array (0 .. 4) of Unsigned_32;

   function "+" (A, B : Hash_Buffer) return Hash_Buffer is
      Result : Hash_Buffer;
   begin
      for I in Result'Range loop
         Result (I) := A(I) + B(I);
      end loop;

      return Result;
   end "+";

   -------------
   -- Padding --
   -------------
   -- This function was modified by AS in Convert_2 and ((Msg'Length)*8)

      function Padding (Msg : Byte_Array) return Byte_Array is
      subtype Length_Buffer is Byte_Array (1 .. 4);


      function Convert (Value : Integer) return Length_Buffer is
         A : Integer := Value;
         B : Length_Buffer;
      begin
         B(4) := Unsigned_8(A mod 256);
         B(3) := Unsigned_8(A / 256);
         B(2) := Unsigned_8(A / (256 ** 2));
         B(1) := Unsigned_8(A / (256 ** 3));
         return B;
      end Convert;


      N_Padding_Bytes : Integer;

   begin
      N_Padding_Bytes := (448 / 8) - Msg'Length mod (512 / 8);
      if (N_Padding_Bytes <= 0) then
         N_Padding_Bytes := (512 / 8) + N_Padding_Bytes;
      end if;

      N_Padding_Bytes := N_Padding_Bytes + (64 / 8);

      declare
         Result : Byte_Array (1 .. N_Padding_Bytes) := (others => 0);
      begin
         Result (Result'First) := 2#1000_0000#;
         Result (Result'Last-3 .. Result'Last) := Convert((Msg'Length) * 8);
         return Result;
      end;
   end Padding;

   ----------
   -- Sha1 --
   ----------

   function Sha1 (Msg : Byte_Array) return Sha1_Hash is

      Init : constant Hash_Buffer := (0 => 16#6745_2301#,
                                      1 => 16#EFCD_AB89#,
                                      2 => 16#98BA_DCFE#,
                                      3 => 16#1032_5476#,
                                      4 => 16#C3D2_E1F0#);



      function Digest (X : Byte_Array; T : Hash_Buffer) return Hash_Buffer is
         type Expanded_Chunk_Idx is new Integer range 0 .. 79;
         type Expanded_Chunk is array (Expanded_Chunk_Idx) of Unsigned_32;

         procedure Expand (Input  : in     Byte_Array;
                           Output :    out Expanded_Chunk) is

            subtype Buf32 is Byte_Array(1 .. 4);

            function Convert (W : Buf32) return Unsigned_32 is
               R : Unsigned_32 := 0;

            begin
               R := Unsigned_32(W(1)) * (2 ** 24) + Unsigned_32(W(2)) * (2 ** 16)
                     + Unsigned_32(W(3)) * (2 ** 8) + Unsigned_32(W(4));
               return R;
            end Convert;

            Pos_In  : Integer := Input'First;
            Pos_Out : Expanded_Chunk_Idx := Output'First;

         begin
            pragma Assert (X'Length = 16 * 32 / 8);

            while (Pos_In < Input'Last) loop
               Output (Pos_Out) := Convert (Input (Pos_In .. Pos_In + 3));
               Pos_In  := Pos_In + 4;
               Pos_Out := Pos_Out + 1;
            end loop;


            for I in Pos_Out .. Output'Last loop
               Output (I) := Output (I - 3) xor Output (I - 8);
               Output (I) := Output (I)     xor Output (I - 14);
               Output (I) := Output (I)     xor Output (I - 16);
               Output (I) := Rotate_Left(Output(I), 1);
            end loop;
         end Expand;

         Result : Hash_Buffer := T;
         Buffer : Expanded_Chunk := (Others => 0);
         A : constant Integer := Result'First;
         B : constant Integer := Result'First + 1;
         C : constant Integer := Result'First + 2;
         D : constant Integer := Result'First + 3;
         E : constant Integer := Result'First + 4;

         F : Unsigned_32;
         Temp : Unsigned_32;
      begin
         Expand(X, Buffer);
         for I in Buffer'Range loop
            case I is
               when 0 .. 19 =>
                  F := (Result (B) and Result (C)) or
                    ((not Result (B)) and Result (D));
                  F := F + 16#5A82_7999#;
               when 20 .. 39 =>
                  F := Result (B) xor Result (C) xor Result (D);
                  F := F + 16#6ED9_EBA1#;
               when 40 .. 59 =>
                  F := (Result (B) and Result (C)) or
                    (Result (B) and Result (D)) or
                    (Result (C) and Result (D));
                  F := F + 16#8F1B_BCDC#;
               when 60 .. 79 =>
                  F := Result (B) xor Result (C) xor Result (D);
                  F := F + 16#CA62_C1D6#;
            end case;

            Temp := Rotate_Left (Result (A), 5) + Result (E) + F + Buffer (I);
            Result (E) := Result (D);
            Result (D) := Result (C);
            Result (C) := Rotate_Left(Result (B), 30);
            Result (B) := Result (A);
            Result (A) := Temp;

         end loop;

         return Result;
      end Digest;

      Padded_Msg : Byte_Array := Msg & Padding (Msg);
      Pos        : Integer     := Padded_Msg'First;
      Chunk_Len  : constant Positive := 512 / 8;

      Result : Hash_Buffer := Init;

   begin
      pragma Assert (Padded_Msg'Length mod Chunk_Len = 0);

      while Pos < Padded_Msg'Last loop
         Result := Result + Digest (Padded_Msg (Pos .. Pos + Chunk_Len - 1),Result);
         Pos := Pos + Chunk_Len;
      end loop;

      declare
         function To_Sha1_Hash (A : in Hash_Buffer) return Sha1_Hash is
            J : Integer;
            C : Hash_Buffer := A;
            B : Sha1_Hash;

         begin
            J := B'First;
            for I in C'Range
            loop
               B(J) := Unsigned_8(C(I) / 2 ** 24);
               C(I) := C(I) mod 2 ** 24;
               B(J+1) := Unsigned_8(C(I) / 2 ** 16);
               C(I) := C(I) mod 2 ** 16;
               B(J+2) := Unsigned_8(C(I) / 2 ** 8);
               C(I) := C(I) mod 2 ** 8;
               B(J+3) := Unsigned_8(C(I));
               J := J + 4;
            end loop;
            return B;
         end To_Sha1_Hash;

      begin



         return To_Sha1_Hash (Result);
      end;
   end Sha1;

   function Sha1 (Msg : String) return Sha1_Hash is
      subtype Input_Type  is String(Msg'Range);
      subtype Buffer_Type is Byte_Array(Msg'Range);

      function Str_To_Bytes is
         new Ada.Unchecked_Conversion (Source => Input_Type,
                                       Target => Buffer_Type);
   begin
      return SHA1(Str_To_Bytes(Msg));
   end SHA1;
end Crypt.SHA1;

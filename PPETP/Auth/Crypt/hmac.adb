package body HMAC is
   I_Pad : Block_Type := (others => 16#36#);
   O_Pad : Block_Type := (others => 16#5c#);

   -----------
   -- "xor" --
   -----------

   function "xor" (A, B : Block_Type) return Block_Type is
      Result : Block_Type;
   begin
      for I in Result'Range loop
         Result (I) := A (I) xor B (I);
      end loop;

      return Result;
   end "xor";

   ---------------
   -- Normalize --
   ---------------

   -- Force Key length to Block_Size, according to the procedure
   -- proposed in FIPS 198:
   --
   --   + If Key length is Block_Size, use Key as it is
   --   + If Key is shorter, pad it with zeros
   --   + If Key is longer, hash it and pad the result with zeros
   --
   function Normalize (Key : Msg_Type) return Block_Type
   is
      function Padded (X : Msg_Type) return Block_Type
      is
         Padding : Msg_Type (X'Length + 1 .. Block_Type'Length) :=
                     (others => 0);
      begin
         return Block_Type (X & Padding);
      end Padded;

   begin
      if (Key'Length = Block_Size) then
         return Block_Type (Key);
      elsif (Key'Length < Block_Size) then
         return Padded (Key);
      else
         return Padded (Msg_Type (Basic_Hash (Key)));
      end if;

   end Normalize;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Msg_Type;
                  Msg : Msg_Type)
                  return Digest_Output is
      True_Key : Block_Type := Normalize (Key);
      Key_I    : Block_Type := True_Key xor I_Pad;
      Key_O    : Block_Type := True_Key xor O_Pad;
   begin
      return Basic_Hash (Key_O & Msg_Type (Basic_Hash (Key_I & Msg)));
   end Hash;

   ----------------
   -- New_Hasher --
   ----------------

   function New_Hasher (Key : Msg_Type) return Hasher is
      True_Key : Block_Type := Normalize (Key);
   begin
      return (Key_I => True_Key xor I_Pad,
              Key_O => True_Key xor O_Pad);
   end New_Hasher;

   ----------
   -- Hash --
   ----------

   function Hash (H   : Hasher;
                  Msg : Msg_Type)
                  return Digest_Output is
      subtype MT is Msg_Type;
   begin
      return Basic_Hash (H.Key_O & Msg_Type (Basic_Hash (H.Key_I & Msg)));
   end Hash;

end HMAC;

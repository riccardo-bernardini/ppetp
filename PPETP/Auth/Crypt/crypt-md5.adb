with Ada.Unchecked_Conversion;

with GNAT.MD5;
with Ada.Streams;    use Ada.Streams;

package body Crypt.MD5 is
   package Basic_MD5 renames GNAT.MD5;

   function To_Hash_Type (X : Basic_MD5.Message_Digest)
                         return Hash_Type is

      subtype Hex_Byte is String(1..2);

      function Hex_To_Byte (Hex : Hex_Byte) return Unsigned_8
      is
         function Hex_Value (C : Character) return Unsigned_8
         is
         begin
            case C is
               when '0'..'9' =>
                  return Character'Pos(C) - Character'Pos('0');
               when 'a' .. 'f' =>
                  return Character'Pos(C) - Character'Pos('a');
               when 'A' .. 'F' =>
                  return Character'Pos(C) - Character'Pos('A');
               when others =>
                  raise Constraint_Error;
            end case;
         end Hex_Value;
      begin
         return Hex_Value(Hex(2))+16*Hex_Value(Hex(1));
      end Hex_To_Byte;

      Result : Hash_Type;
      Pos    : Integer;
   begin
      pragma Assert(Result'Length = Basic_MD5.Message_Digest'Length / 2);

      Pos := X'First;
      for I in Result'Range loop
         Result(I) := Hex_To_Byte (X (Pos .. Pos + 1));
         Pos := Pos + 2;
      end loop;

      return Result;
   end To_Hash_Type;

   function Digest (Msg : Msg_Type)
                   return Hash_Type is
      subtype MD5_Buf is Stream_Element_Array(1..Msg'Length);
      subtype Msg_Buf is Msg_Type(Msg'First..Msg'Last);

      function To_MD5 is
         new Ada.Unchecked_Conversion(Source => Msg_Buf,
                                      Target => MD5_Buf);

   begin
      return To_Hash_Type (Basic_MD5.Digest (To_MD5 (Msg)));
   end Digest;
end  Crypt.MD5;

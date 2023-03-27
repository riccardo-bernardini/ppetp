with Str_Conversion;
with Ada.Strings.Bounded;    use Ada.Strings.Bounded;
with Text_Io; use Text_Io;
package body Network_Utilities is

   function To_String is
      new Str_Conversion.Modular_To_String(Unsigned_8);

   function To_String is
      new Str_Conversion.Modular_To_String(Unsigned_16);



   function "<" (Left, Right : Sock_Addr_Type) return Boolean is
   begin
      if Left.Family < Right.Family then
         return True;
      elsif Left.Family > Right.Family then
         return False;
      else

         if Left.Family = Family_Inet then
            declare
               left_ip  : Inet_Addr_V4_Buffer;
               right_ip : Inet_Addr_V4_Buffer;
            begin

               left_ip  := To_Array(Left.Addr);
               right_ip := To_Array(Right.Addr);

               -- check the ip
               for i in 1 .. 4 loop
                  if left_ip(i) < right_ip(i) then
                     return true;
                  elsif left_ip(i) > right_ip(i) then
                     return false;
                  else
                     null;
                  end if;
               end loop;

               --same ip, check the port
               return Left.Port < Right.Port;
            end;
         else
            declare
               left_ip  : Inet_Addr_V6_Buffer;
               right_ip : Inet_Addr_V6_Buffer;
            begin
               left_ip  := To_Array(Left.Addr);
               right_ip := To_Array(Right.Addr);

               -- check the ip
               for i in 1 .. 16 loop
                  if left_ip(i) < right_ip(i) then
                     return true;
                  elsif left_ip(i) > right_ip(i) then
                     return false;
                  else
                     null;
                  end if;
               end loop;

               --same ip, check the port
               return Left.Port < Right.Port;

            end;
         end if;

      end if;

   end "<";


   function "=" (Left, Right : Sock_Addr_Type) return Boolean is
   begin
      return (Left.Addr = Right.Addr) and (Left.Port = Right.Port);
   end "=";



   ---------------------
   -- To_Dot_Notation --
   ---------------------

   function To_Dot_Notation (Buf : Inet_Addr_VN_Buffer) return String is

      -- Max length in dot notation:
      -- IPv6  --> 8 4-char words + 7 ':' separator = 39
      package Ip_Strings is
        new Generic_Bounded_Length (Max => 39);

      use Ip_Strings;

      Result : Bounded_String := To_Bounded_String ("");
   begin
      if (Buf'Length = Inet_Addr_V4_Buffer'Length) then
         for I in Buf'Range loop
            Result := Result & To_String (Buf (I), 10);
            if (I < Buf'Last) then
               Result := Result & ".";
            end if;
         end loop;
      elsif (Buf'Length = Inet_Addr_V6_Buffer'Length) then
         declare
            I : Integer := Buf'First;
            Word : Unsigned_16;
         begin
            while I <= Buf'Last-1 loop
               if (I > Buf'First) then
                  Result := Result & ":";
               end if;

               Word := Unsigned_16(Buf(I))*256 + Unsigned_16(Buf(I+1));
               Result := Result & To_String (Word, 16);
               I := I + 2;
            end loop;
         end;
      else
         raise Constraint_Error;
      end if;

      return To_String (Result);
   end To_Dot_Notation;

   ---------------
   -- Inet_Addr --
   ---------------

   function Inet_Addr
     (Buffer : Inet_Addr_VN_Buffer)
      return Inet_Addr_Type
   is
   begin
      if (Buffer'Length /= Inet_Addr_V4_Buffer'Length and
            Buffer'Length /= Inet_Addr_V6_Buffer'Length) then
         raise Constraint_Error;
      end if;

      return Inet_Addr (To_Dot_Notation (Buffer));
   end Inet_Addr;

   function Str_To_Integer is
      new Str_Conversion.String_To_Integer(Integer);


   function Inet_To_Array (Addr : Inet_Addr_Type)
                           return Inet_Addr_VN_Buffer is
      Str    : String := Image (Addr) & ".";
      Result : Inet_Addr_V4_Buffer;
      Pos    : Integer := Result'First;
      Start  : Integer := Str'First;
      Buf    : Integer;
   begin
      -- Put_Line ("<" & Str & ">");

      for I in Str'Range loop
         if (Str (I) = '.') then
            Buf := Str_To_Integer (Str (Start .. I - 1), 10);
            if (Buf < 0 or Buf > 255) then
               raise Constraint_Error;
            end if;

            Result (Pos) := Byte (Buf);
            Start := I + 1;
            Pos   := Pos + 1;
         end if;
      end loop;
      pragma Assert (Pos = Result'Last + 1);

      return Result;
   end Inet_To_Array;

   --------------------
   -- Inet6_To_Array --
   --------------------

   function Inet6_To_Array (Addr : Inet_Addr_Type)
                           return Inet_Addr_VN_Buffer is
      Str    : String := Image (Addr) & ":";
      Result : Inet_Addr_V6_Buffer;
      Pos    : Integer := Result'First;
      Start  : Integer := Str'First;
      Buf    : Integer;
   begin
      for I in Str'Range loop
         if (Str (I) = ':') then
            Buf := Str_To_Integer (Str (Start .. I - 1), 16);
            if (Buf < 0 or Buf > 65535) then
               raise Constraint_Error;
            end if;

            Result (Pos) := Byte (Buf / 256) ;
            Result (Pos+1) := Byte (Buf mod 256) ;
            Start := I + 1;
            Pos   := Pos + 2;
         end if;
      end loop;
      pragma Assert (Pos = Result'Last + 1);

      return Result;
   end Inet6_To_Array;



   --------------
   -- To_Array --
   --------------

   function To_Array (Addr : Inet_Addr_Type)
                      return Inet_Addr_VN_Buffer is
   begin
      case Addr.Family is
         when Family_Inet =>
            return Inet_To_Array (Addr);
         when Family_Inet6 =>
            return Inet6_To_Array (Addr);
      end case;
   end To_Array;

   ------------------
   -- To_Inet_Addr --
   ------------------

   function To_Inet_Addr (S : String)
                         return Inet_Addr_Type is
   begin
      return Inet_Addr(S);
   exception
      when Socket_Error =>
         raise Invalid_IP_Address;
   end To_Inet_Addr;
end Network_Utilities;

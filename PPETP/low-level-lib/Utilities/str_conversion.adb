with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

package body Str_Conversion is

   ---------------------
   -- Positive_To_Str --
   ---------------------

   function Positive_To_Str (X    : Positive;
                             Base : Base_Type)
                             return String
   is
      To_Chr  : constant String (1 .. 16) := "0123456789abcdef";
      Result  : Unbounded_String := Null_Unbounded_String;
      Local_X : Natural := X;
   begin
      while Local_X /= 0 loop
         Result  := To_Chr (Local_X mod Base + 1) & Result;
         Local_X := Local_X / Base;
      end loop;

      return To_String (Result);
   end Positive_To_Str;


   -----------------------
   -- Integer_To_String --
   -----------------------

   function Integer_To_String
     (X    : Int;
      Base : Base_Type)
      return String
   is

   begin
      if (X = 0) then
         return "0";
      elsif (X < 0) then
         return "-" & Positive_To_Str (Positive (-X), Base);
      else
         return Positive_To_Str (Positive (X), Base);
      end if;
   end Integer_To_String;

   function Pad_To_Len (S : String; Len : Natural; Padder : Character:='0')
                       return String is
   begin
      if (Len = 0 or S'Length >= Len) then
         return S;
      else
         return To_String((Len - S'Length) * Padder) & S;
      end if;
   end Pad_To_Len;

   -----------------------
   -- Modular_To_String --
   -----------------------

   function Modular_To_String
     (X    : Int;
      Base : Base_Type;
      Len  : Natural := 0)
      return String
   is
      function Basic_Modular_To_String (X    : Int;
                                        Base : Base_Type)
                                       return String is
      begin
         if (X = 0) then
            return "0";
         else
            return Positive_To_Str (Positive (X), Base);
         end if;
      end Basic_Modular_To_String;
   begin
      return Pad_To_Len (Basic_Modular_To_String(X, Base), Len);
   end Modular_To_String;

   function Ch_To_Integer (C : Character;
                           B : Base_Type)
                           return Natural is
      Result : Natural;
   begin
      case C is
         when '0' .. '9' =>
            Result := Character'Pos(C) - Character'Pos('0');
         when 'a' .. 'f' =>
            Result := Character'Pos(C) - Character'Pos('a') + 10;
         when 'A' .. 'F' =>
            Result := Character'Pos(C) - Character'Pos('A') + 10;
         when others =>
            raise Invalid_Number with "Invalid char '" & C & "'";
      end case;

      if (Result >= B) then
         raise Invalid_Number with "Invalid digit '" & C & "'";
      end if;

      return Result;
   end Ch_To_Integer;
   -----------------------
   -- String_To_Integer --
   -----------------------

   function String_To_Integer (X        : String;
                               Base     : Base_Type;
                               On_Error : Error_Policy := Die)
                               return Int is
      Segno : Integer := 1;
      Pos   : Positive := X'First;
      Result : Integer := 0;

      Valid_Head : Boolean := False;
   begin
      while Pos <= X'Last and then X (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if (Pos <= X'Last and then X(Pos) = '-') then
         Segno := -1;
         Pos   := Pos + 1;
      end if;

      if (Pos > X'Last) then
         raise Invalid_Number with "No digit";
      end if;

      begin
         while (Pos <= X'Last) loop
            Result := Base * Result + Ch_To_Integer (X (Pos), Base);
            Pos := Pos + 1;

            -- If I am here, we found at least one valid digit
            Valid_Head := True;
         end loop;
      exception
         when Invalid_Number =>
            if (On_Error = Die or not Valid_Head) then
               raise;
            end if;
      end;

      return Int(Segno * Result);
   end String_To_Integer;

   function String_To_Modular (X        : String;
                               Base     : Base_Type;
                               On_Error : Error_Policy := Die)
                               return Int is
      Pos    : Positive := X'First;
      Result : Int := 0;
      Valid_Head : Boolean := False;
   begin
      while Pos <= X'Last and then X (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if (Pos > X'Last) then
         raise Invalid_Number with "No digit";
      end if;

      begin
         while (Pos <= X'Last) loop
            Result := Int(Base) * Result + Int(Ch_To_Integer (X (Pos), Base));
            Pos := Pos + 1;
            -- If I am here, we found at least one valid digit
            Valid_Head := True;
         end loop;
      exception
         when Invalid_Number =>
            if (On_Error = Die or not Valid_Head) then
               raise;
            end if;
      end;

      return Result;
   end String_To_Modular;
end Str_Conversion;

--        function To_Chr (X : Unsigned_8) return Character is
--        begin
--           case X is
--              when 0 .. 9 =>
--                 return Character'Val (X + Character'Pos ('0'));
--              when 10 .. 15 =>
--                 return Character'Val (X - 10 + Character'Pos ('A'));
--              when others =>
--                 raise Constraint_Error;
--           end case;
--        end To_Chr;

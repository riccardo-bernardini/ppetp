with Str_Conversion;          use Str_Conversion;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;
package body byte_arrays is
   function New_Byte_Array (X : Stream_Element_Array)
                            return Byte_Array_Pt is
      Result : Byte_Array_Pt;
   begin
      Result := new Byte_Array (X'First .. X'Last);
      for I in Result'Range loop
         Result (I) := Byte (X (I));
      end loop;

      return Result;
   end New_Byte_Array;

   function Byte_To_Str is
      new Modular_To_String (Int => Byte);
   ----------
   -- Dump --
   ----------

   procedure Dump (X : Byte_Array; S : String := "") is

   begin
      Put_Line (S & Dump(X));
   end Dump;

   function Dump (X : Byte_Array; C : Boolean := False) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if (X'Length > 100) then
         return "... too long ... (>100)";
      end if;

      Result := Result & " [";
        for I in X'Range loop
           Result := Result & Byte_To_Str (X (I), 16, 2);
           --Put_Line (Byte_To_Str (X (I), 16, 2));
         if (I < X'Last) then
            Result := Result & ", ";
         end if;
      end loop;

      Result := Result & " ]";
      return To_String(Result);
   end Dump;
end byte_arrays;

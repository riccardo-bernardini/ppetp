package body Base_Conversion is
   Digit : constant String := "0123456789ABCDEF";

   procedure To_String (Item     : in     Num;
                        To       :    out String;
                        Base     : in     Number_Base := 16;
                        Zero_Pad : in     Boolean     := True) is
   begin
      if (Zero_Pad) then
         To := (others => '0');
      else
         To := (others => ' ');
      end if;

      declare
         Buffer : Integer := Integer(Item);
         Cursor : Integer := To'Last;
      begin
         while (Buffer > 0) and (Cursor >= To'First) loop
            To(Cursor) := Digit(Buffer mod Base + Digit'First);
            Buffer := Buffer / Base;
            Cursor := Cursor - 1;
         end loop;
      end;
   end To_String;
end Base_Conversion;

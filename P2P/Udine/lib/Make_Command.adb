package body Make_Command is

   function Image(X : String_Array) return String is
      Result : Unbounded_String;
   begin

      for I in X'Range loop
         Result := Result & X(I);
         if (I < X'Last) then
            Result := Result & " ";
         end if;
      end loop;

      return To_String(Result);

   end Image;


   function Make_Command_1(Command    : in String;
                           Parameters : in String_Array) return String is
      Idx         : Unsigned_32;
      Time_Hex    : String(1..6);
      Total1      : String(1..8);
      Output      : Unbounded_String;

   begin
      --        Timestamp_Function(Idx);
      Idx := Next_Timestamp;
      To_Hex(Idx, 6, Time_Hex);
      declare
         Total : String := Command & " " & Time_Hex & " " & Image(Parameters);
      begin
         Create_CRC32(Total, Total1);
         Output := To_Unbounded_String(Total & " " & Total1);
      end;

      return To_String(Output);

   end Make_Command_1;

end Make_Command;






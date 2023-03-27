with Text_Io;                    use Text_Io;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Query_State;                use Query_State;

procedure Mini_DB_Client
   is

begin
   if (Argument_Count < 3) then
      Put_Line("Wrong # arg");
      Set_Exit_Status(Failure);
      return;
   end if;

   declare
      DB_Port  : Positive := Positive'Value(Argument(1));
      DB       : DB_Connection;
      Command  : String := Translate(Source => Argument(2),
                                     Mapping => Upper_Case_Map);
      Var_Name : String := Argument(3);
   begin
      --Put_Line("Conecting to " & Integer'image(Db_Port));

      Connect(Connection => DB,
              Port       => DB_Port);

      if (Command = "SET") then
         if (Argument_Count /= 4) then
            Put_Line("Wrong # arg");
            Set_Exit_Status(Failure);
            return;
         end if;

         Set(DB, Var_Name, Argument(4));
      elsif (Command = "GET") then
         if (Argument_Count /= 3) then
            Put_Line("Wrong # arg");
            Set_Exit_Status(Failure);
            return;
         end if;

         Put_Line (Get(DB, Var_Name));
      else
         Put_Line ("Unrecognized command " & Command);
         Set_Exit_Status(Failure);
         return;
      end if;
   end;
end Mini_DB_Client;

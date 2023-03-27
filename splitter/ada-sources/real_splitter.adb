with Text_Io;
with Ada.Command_Line;
with Splitter_Lib.Command_Parser;
with Splitter_Lib.Splitter;
with Splitter_Lib.Controller;

use Splitter_Lib;

procedure Real_Splitter is

   Config : Command_Parser.Config_Data;
begin
   Command_Parser.Parse_Command_Line(Config);
   Splitter.Start(Config);
exception
   when Command_Parser.Bad_Command =>
      Text_Io.Put_Line("Bad syntax");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when Controller.Server_Not_Found =>
      Text_Io.Put_Line("Could not connect to control server");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Real_Splitter;

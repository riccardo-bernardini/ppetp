package body Profiles.Config.Basic is

   -----------------------
   -- New_Config_Parser --
   -----------------------
   function New_Config_Parser return Config_Parser_Pt is
   begin
      return new Config_Parser;
   end New_Config_Parser;

   -----------
   -- Parse --
   -----------
   procedure  Parse (Parser : in out Config_Parser;
                     Table  : in     Config_Table;
                     Result :    out Parameters_Class_Pt;
                     Errors : in out Config_Error_List) is
   begin
      for I in 1 .. Size(Table) loop
         Errors.Append ((Name   => To_Unbounded_String(Get_Name(Table, I)),
                         Value  => To_Unbounded_String(Get_Value(Table, I)),
                         Reason => Param_Unknown));
      end loop;

      Result := null;
   end Parse;

end Profiles.Config.Basic;

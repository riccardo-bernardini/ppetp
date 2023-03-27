package  Profiles.Config.Basic is
   -- ========================== --
   -- == Configuration Parser == --
   -- ========================== --

   type Config_Parser is new Root_Config_Parser with null record;
   type Config_Parser_Pt is access Config_Parser;

   function New_Config_Parser return Config_Parser_Pt;

   overriding
   procedure  Parse (Parser : in out Config_Parser;
                     Table  : in     Config_Table;
                     Result :    out Parameters_Class_Pt;
                     Errors : in out Config_Error_List);



end Profiles.Config.Basic;

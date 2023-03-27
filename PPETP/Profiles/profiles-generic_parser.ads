with Profiles.Parameters;    use Profiles.Parameters;
with Profiles.Config;        use Profiles.Config;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

generic
   type Param_Name is (<>);
   type Parameters_Type is new Root_Parameters with private;
   type Result_Pt is access Parameters_Type;
package Profiles.Generic_Parser is

   type Config_Parser_Callback is
     access procedure (Name   : in     String;
                       Value  : in     String;
                       Result :    out Parameters_Type;
                       Ok     :    out Boolean;
                       Reason :    out Config_Err_Reason);


   type Parser_Info_Entry is
      record
         Name      : Unbounded_String;
         Callback  : Config_Parser_Callback;
         Mandatory : Boolean;
      end record;

   type Parser_Info_Table is
     array (Param_Name) of Parser_Info_Entry;

   procedure  Parse (Info_Table : in Parser_Info_Table;
                     Table      : in     Config_Table;
                     Result     :    out Parameters_Class_Pt;
                     Errors     : in out Config_Error_List);
end Profiles.Generic_Parser;


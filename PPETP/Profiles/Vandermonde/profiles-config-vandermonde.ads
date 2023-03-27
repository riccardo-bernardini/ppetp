package Profiles.Config.Vandermonde is

   type Config_Parser is new Root_Config_Parser with null record;
   type Config_Parser_Pt is access Config_Parser;

   -- Valid Vandermonde Parameters
   type Valid_Vandermonde_Parameters is (REDUCTION_FACTOR,
                                         GALOIS_ELEMENT,
                                         GF_SIZE);

   type Max_Range is range 0 .. 2**32-1;

   function New_Config_Parser return Config_Parser_Pt;

   overriding
   procedure  Parse (Parser : in out Config_Parser;
                     Table  : in     Config_Table;
                     Result :    out Parameters_Class_Pt;
                     Errors : in out Config_Error_List);

end Profiles.Config.Vandermonde;

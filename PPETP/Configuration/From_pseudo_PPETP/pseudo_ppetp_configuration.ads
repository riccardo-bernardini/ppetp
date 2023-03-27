--
with Pseudo_Ppetp;                use Pseudo_PPETP;
with PPETP;			use PPETP;

package Pseudo_Ppetp_Configuration is
   type Configuration_Format is (No_Format, Address_List);

   -- Configure the PPETP session identified by ID by downloading
   -- a configuration file from URL
   procedure Configure(ID  : Session_ID;
                       URL : String);

   -- Configure the PPETP session identified by ID by using the
   -- configuration string in Config_Data.  The format of Config_Data
   -- is identified by Data_Format.
   procedure Configure(ID          : Session_ID;
                       Config_Data : String;
                       Data_Format : Configuration_Format);

   -- Configure the PPETP session identified by ID by using the
   -- configuration string in Config_Data.  The format of Config_Data
   -- is identified by its MIME name in MIME_Name.
   procedure Configure(ID          : Session_ID;
                       Config_Data : String;
                       MIME_Name   : String);

   -- Map a MIME_Name to the corresponding Configuration_Format.
   -- Return Invalid if the name is not recognized.
   function MIME_String_To_Format (Mime : String)
                                   return Configuration_Format;

   Unrecognized_MIME_Name : exception;
   Invalid_Config_Format  : exception;
end Pseudo_Ppetp_Configuration;


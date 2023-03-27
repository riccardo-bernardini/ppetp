with GNAT.Sockets;                   use GNAT.Sockets;
with Ada.Strings;                    use Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with URL_Downloaders;                use URL_Downloaders;

with PPETP;			     use PPETP;
with PPETP.API;			     use PPETP.API;


package body Pseudo_Ppetp_Configuration is
   type MIME_Descriptor is record
      MIME_Name : Unbounded_String;
      Format    : Configuration_Format;
   end record;

   function U_Str (Source : String) return Unbounded_String
                   renames To_Unbounded_String;

   type MIME_Descriptor_Array is
     array (Natural range <>) of MIME_Descriptor;

   MIME_To_Format_Table : constant MIME_Descriptor_Array :=
                            (1 =>
                               (MIME_Name => U_Str ("x-ppetp-config/address-list"),
                                Format    => Address_List)
                            );

   function MIME_String_To_Format (Mime : String)
                                   return Configuration_Format
   is

   begin
      -- Put_Line("mime: [" & Mime & "]");
      for I in MIME_To_Format_Table'Range loop
         --Put_Line ("trying: [" & To_String (MIME_To_Format_Table (I).MIME_Name) & "]");
         if To_String(MIME_To_Format_Table (I).MIME_Name) = Mime then
            return MIME_To_Format_Table (I).Format;
         end if;
      end loop;

      raise Unrecognized_MIME_Name;
   end MIME_String_To_Format;

   procedure Config_Via_Address_List (ID          : Session_ID;
                                      Config_Data : String) is
      Port_Char_Set    : constant Character_Set := To_Set ("0123456789");
      Channel_Char_Set : constant Character_Set := Port_Char_Set;
      Host_Char_Set    : constant Character_Set := Port_Char_Set or To_Set ('.');

      Host_Start    : Natural;
      Host_End      : Natural;
      Port_Start    : Natural;
      Port_End      : Natural;
      Ch_Start	    : Natural;
      Ch_End	    : Natural;
      Cursor        : Natural;
   begin
      Cursor := Config_Data'First;
      loop
         exit when Cursor > Config_Data'Last;

         Fixed.Find_Token (Source => Config_Data(Cursor .. Config_Data'Last),
                           Set    => Host_Char_Set,
                           Test   => Inside,
                           First  => Host_Start,
                           Last   => Host_End);

         exit when Host_End = 0;
         Cursor := Host_End + 1;

         if (Cursor > Config_Data'Last) then
            raise Program_Error;
         end if;

         Fixed.Find_Token (Source => Config_Data(Cursor .. Config_Data'Last),
                           Set    => Port_Char_Set,
                           Test   => Inside,
                           First  => Port_Start,
                           Last   => Port_End);

         if (Port_End = 0) then
            raise Program_Error;
         end if;

         Cursor := Port_End + 1;

         if (Cursor > Config_Data'Last) then
            raise Program_Error;
         end if;

         Fixed.Find_Token (Source => Config_Data(Cursor .. Config_Data'Last),
                           Set    => Channel_Char_Set,
                           Test   => Inside,
                           First  => Ch_Start,
                           Last   => Ch_End);

         if (Ch_End = 0) then
            raise Program_Error;
         end if;

         Cursor := Ch_End + 1;

         declare
            Host_Name : String := Config_Data (Host_Start .. Host_End);
            Port      : Port_Type := Port_Type'Value (Config_Data (Port_Start .. Port_End));
            Channel   : PPETP_Channel_ID := PPETP_Channel_ID'Value (Config_Data (Ch_Start .. Ch_End));

         begin

            Connect(Session   => ID,
                    Peer_Addr => Inet_Addr(Host_Name),
                    Port      => Port,
                    Channel   => Channel );

         end;
      end loop;
   end Config_Via_Address_List;

   -- Configure the PPETP session identified by ID by using the
   -- configuration string in Config_Data.  The format of Config_Data
   -- is identified by Data_Format.
   procedure Configure (ID          : Session_ID;
                        Config_Data : String;
                        Data_Format : Configuration_Format) is
   begin
      case Data_Format is
         when No_Format =>
            raise Invalid_Config_Format;
         when Address_List =>
            Config_Via_Address_List(ID, Config_Data);
      end case;
   end Configure;

   procedure Configure (ID          : Session_ID;
                        Config_Data : String;
                        MIME_Name   : String) is
   begin
      Configure (ID, Config_Data, MIME_String_To_Format (MIME_Name));
   end Configure;

   procedure Configure(ID  : Session_ID;
                       URL : String) is
      Content_Type : Unbounded_String;
      Downloader   : URL_Downloader;
      Config_Data  : Unbounded_String;
   begin
      Get (Downloader, URL);

      Configure (ID          => ID,
                 MIME_Name   => Header (Downloader, "content-type"),
                 Config_Data => Content (Downloader));
   exception
      when No_Such_Header =>
         raise Invalid_Config_Format;
   end Configure;
end Pseudo_Ppetp_Configuration;

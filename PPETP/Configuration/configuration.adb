with Profiles;         use  Profiles;
with Text_Io;          use  Text_Io;
with Auth.Credentials; use Auth.Credentials;
with PPETP;	       use PPETP;

package body Configuration is

   function "=" (L, R : Server_Config) return Boolean is
   begin
      return L.Addr = R.Addr
          and L.Port = R.Port
          and L.Id = R.Id;
   end "=";


   function "=" (L, R : Channel_Config) return Boolean is
      use type Profiles.Parameters.Parameters_Class_Pt;
   begin
      if (L.Id /= R.Id
          or L.Profile /= R.Profile
          or ((L.Parameters = null) /= (R.Parameters = null))) then
         return False;
      end if;

      if (L.Parameters = null) then
         return True;
      else
         raise Program_Error with "Unimplemented";
      end if;
   end "=";

   function "=" (L, R : Input_Config) return Boolean is
   begin
      if (L.Id /= R.Id
          or L.Addr_Type /= R.Addr_Type
          or L.Addr /= R.Addr
          or L.Port /= R.Port
          or (L.Auth = null) /= (R.Auth = null)) then
         Put_Line ("input 1");
         return False;
      end if;

      -- Here L.auth = null if and only if R.auth = null
      return (L.Auth = null) or else (L.Auth.all = R.Auth.all);
   end "=";

   function "=" (L, R : Input_Conf_Array)return Boolean is
   begin
      if (L'First /= R'First or L'Last /= R'Last) then
         return False;
      end if;

      for I in L'Range loop
         if (L (I) /= R (I)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   function "=" (L, R : Output_Conf_Array)return Boolean is
   begin
      if (L'First /= R'First or L'Last /= R'Last) then
         return False;
      end if;

      for I in L'Range loop
         if (L (I) /= R (I)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   function "=" (L, R : session_config) return Boolean is
   begin
      return L.Server = R.Server
        and L.Inputs = R.Inputs
        and L.Outputs = R.Outputs;
   end "=";

   generic
      type Element is private;
      type Element_Array is array (Positive range <>) of Element;
      Name : String;
      with procedure Dump (X : Element; Label : String; Tabbing : Natural)
        is <>;
   procedure Dump_Array (X       : Element_Array;
                         Label   : String := "";
                         Tabbing : Natural := 0);

   procedure Dump_Array (X       : Element_Array;
                         Label   : String := "";
                         Tabbing : Natural := 0) is
      Tab : String (1 .. Tabbing) := (others => ' ');
   begin
      if (Label /= "") then
         Put_Line (Tab & Label);
      end if;

      if (X'Length = 0) then
         Put_Line (Tab & "[]");
      else
         for I in X'Range loop
            Dump (X (I), Name & "(" & Integer'Image (I) & ")", Tabbing + 1);
         end loop;
      end if;
   end Dump_Array;

   procedure Dump (X       : Command_Auth_Data;
                   Label   : String  := "";
                   Tabbing : Natural := 0) is
      Tab : String (1 .. Tabbing) := (others => ' ');
   begin
      if (Label /= "") then
         Put_Line (Tab & Label);
      end if;

      Put_Line (Tab & "command: " & Command_Type'Image (X.Command));
      Put_Line (Tab & "data: " & Image(X.Credentials));
   end Dump;

   procedure Dump is
     new Dump_Array (Element       => Command_Auth_Data,
                     Element_Array => Auth_Array,
                     Name          => "auth:");

   procedure Dump (X       : Input_Config;
                   Label   : String  := "";
                   Tabbing : Natural := 0) is
      Tab : String(1..Tabbing) := (others => ' ');
      Channels_Str: Unbounded_String := To_Unbounded_String("");
   begin
      if (Label /= "") then
         Put_Line (Tab & Label);
      end if;

      Put_Line (Tab & "id: " & X.Id'img );
      Put_Line (Tab & "addr-type: " & X.Addr_Type'img );

      for i in 1.. X.Channels'length loop
         Channels_Str := Channels_Str & X.Channels(i).Id'img & ",";
      end loop;
      Put_Line (Tab & "Channels: [" & To_String(Channels_Str) & "]" );
      Put_Line (Tab & "addr: " & Image (X.Addr) & ":"
                & Port_Type'Image (X.Port));
      if (X.Auth = null) then
         Put_Line ("auth: void");
      else
         Dump(X.Auth.all, "auth: ", Tabbing + 2);
      end if;
   end Dump;


   procedure Dump (X       : Channel_Config;
                   Label   : String  := "";
                   Tabbing : Natural := 0) is
        Tab : String(1..Tabbing) := (others => ' ');
   begin
      Put(Tab);
      if (Label /= "") then
         Put (Label);
      end if;

      Put ("[ID, proof, param]=");

      Put("[" & X.Id'img);
      Put (", " & Profile_Type'Image (X.Profile));
      if (X.Parameters /= null) then
         Put (", (" & Image (X.Parameters.all) & ")");
      else
         Put (", ()");
      end if;

      Put_Line("]");
   end Dump;

   procedure Dump (X       : Server_Config;
                   Label   : String := "";
                   Tabbing : Natural := 0) is
      Tab : String(1..Tabbing) := (others => ' ');
   begin

      Put(Tab);
      if (Label /= "") then
         Put (Label);
      end if;

      New_Line;
      Put_Line (Tab & "addr: " & Image (X.Addr) & ":"
                & Port_Type'Image (X.Port));

      Put_Line (Tab & "Id:   " & Integer'Image (X.Id));

   end Dump;

   procedure Dump is
     new Dump_Array (Element       => Channel_Config,
                     Element_Array => Output_Conf_Array,
                     Name          => "Out");

   procedure Dump is
     new Dump_Array (Element       => Input_Config,
                     Element_Array => Input_Conf_Array,
                     Name          => "In");


   procedure Dump(X       : Session_Config;
                  Label   : String := "";
                  Tabbing : Natural := 0) is
      Tab : String (1 .. Tabbing) := (others => ' ');
   begin
      if (Label /= "") then
         Put_Line (Tab & Label);
      end if;

      Put_Line (Tab & "profile = " & Profile_Type'Image (X.Profile) );
      if X.Parameters = null then
         Put_Line(Tab & "Param: []");
      else
         Put_Line(Tab & "Param: [" & Image(X.Parameters.all) & "]");
      end if;

      if X.Server /= null then
         Dump (X.Server.all,  "Server:",  Tabbing => Tabbing+2);
      else
         Put_Line(Tab & "Server: NULL");
      end if;


      if X.Inputs /= null then
         Dump (X.Inputs.all,  "Inputs:",  Tabbing => Tabbing+2);
      else
         Put_Line(Tab & "Inputs: NULL");
      end if;

      if X.Outputs /= null then
         Dump (X.Outputs.all, "Outputs:", Tabbing => Tabbing+2);
      else
         Put_Line(Tab & "Outputs: NULL");
      end if;
   end Dump;

   procedure Dump (X       : Session_Config_Pt;
                   Label   : String := "";
                   Tabbing : Natural := 0) is
   begin
	Dump(X.all, Label, Tabbing);
   end Dump;

   procedure Dump (X       : Config_Data;
                   Label   : String := "";
                   Tabbing : Natural := 0) is
      procedure Sub_Dump is
        new Dump_Array (Element       => Session_Config_Pt,
                        Element_Array => Config_Data,
                        Name          => "Session");
   begin
      Sub_Dump(X, Label, Tabbing);
   end Dump;


end configuration;

with Configuration.XML;       use Configuration, Configuration.XML;
with Test_Report;             use Test_Report;
with Network;                 use Network;
with Profiles;                use Profiles;
with Byte_Arrays;             use Byte_Arrays;
with Auth.Credentials;        use Auth.Credentials;
with Auth.Profiles;           use Auth.Profiles;
with Profiles.Parameters;
with Ada.Text_IO;             use  Ada.Text_IO;
with Ada.Exceptions;          use Ada.Exceptions;

procedure Config_Parser_Test is
   type String_Pt      is access String;
   type Config_Data_Pt is access Config_Data;


   type Test_Case is
      record
         Input  : String_Pt;
         Output : Config_Data_Pt;
      end record;

   type Test_Case_Array is array (Positive range <>) of Test_Case;

   String_1 : constant String :=
     "<configuration>" &
     "<session port=""23671""> " &
     "  <profile name=""basic"" />" &
     "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""1"">" &
     "      <profile name=""basic""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile name=""token"" />" &
     "        <auth-token command=""start""  value=""1234567890abcdef"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
   "</configuration>";

   String_2 : constant String :=
     "<configuration>" &
     "<session port=""23671""> " &
     "  <profile name=""basic"" />" &
     "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""1"">" &
     "      <profile id=""0""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile id=""1"" />" &
     "        <auth-token command=""start""  value=""1234567890abcdef"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
   "</configuration>";

   String_Bad_1 : constant String :=
     "<configuration>" &
     "<session port=""23671""> " &
     "  <profile name=""basic"" />" &
     "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""1"">" &
     "      <profile id=""0""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile id=""1"" />" &
     "        <auth-token command=""start""  value=""1234567890abcde"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
     "</configuration>";

   String_Bad_2 : constant String :=
     "<configuration>" &
     "<session port=""23671""> " &
     "  <profile name=""basic"" />" &
     "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""x"">" &
     "      <profile name=""basic""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile name=""token"" />" &
     "        <auth-token command=""start""  value=""1234567890abcdef"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
   "</configuration>";

   String_Bad_3 : constant String :=
     "<configuration>" &
   "<session port=""23671""> " &
      "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""1"">" &
     "      <profile name=""bas""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile name=""token"" />" &
     "        <auth-token command=""start""  value=""1234567890abcdef"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
   "</configuration>";

   String_Bad_4 : constant String :=
     "<configuration>" &
   "<session port=""23671""> " &
      "  <server address=""131.10.93.123"" port=""12311"" id=""12"" />" &
     "  <output>" &
     "    <channel id=""1"">" &
     "      <profile name=""basic""/>" &
     "    </channel>" &
     "  </output>" &
     "  <input>" &
     "    <peer>" &
     "      <address  address=""192.128.64.8"" port=""32499"" channel=""1"" />" &
     "    </peer>" &
     "    <peer>" &
     "      <address  address=""32.10.9.6"" port=""5213"" channel=""5"" />" &
     "      <auth-data>" &
     "        <auth-profile name=""tk"" />" &
     "        <auth-token command=""start""  value=""1234567890abcdef"" />" &
     "        <auth-token command=""stop""  value=""aabbccddeeff"" />" &
     "      </auth-data>" &
     "    </peer>" &
     "  </input>" &
     "</session> " &
   "</configuration>";
   Key_Start_1_1 : constant Byte_Array := (16#12#, 16#34#, 16#56#, 16#78#,
                                           16#90#, 16#Ab#, 16#Cd#, 16#EF#);

   Key_Stop_1_1  : constant Byte_Array := (16#AA#, 16#BB#, 16#CC#,
                                           16#DD#, 16#EE#, 16#FF#);

   Auth_Empty : constant Auth_Array_Pt := new Auth_Array(1 .. 0);

   Auth_1_1 : constant Auth_Array_Pt := new Auth_Array'
     (1 => (Command => Start,
            Credentials => New_Credential (Data    => Key_Start_1_1,
                                           Profile => Token_Profile)),
      2 => (Command => Stop,
            Credentials => New_Credential (Data    => Key_Stop_1_1,
                                           Profile => Token_Profile))
     );

   Input_1_1 : constant Input_Config := (Addr    => Inet_Addr("192.128.64.8"),
                                         Port    => 32499,
                                         Ch      => 1,
                                         Auth    => null);

   Input_1_2 : constant Input_Config := (Addr    => Inet_Addr("32.10.9.6"),
                                         Port    => 5213,
                                         Ch      => 5,
                                         Auth    => Auth_1_1);

   Output_1_1 : constant Channel_Config := (Id         => 1,
                                            Profile    => Profiles.Basic_Profile,
                                            Parameters => null,
                                            Auth       => null);

   Server_1_1 : constant Server_Config := (Addr => Inet_Addr("131.10.93.123"),
         				   Port => 12311,
				           Id   => 12);

   Session_1_1 : constant Session_Config := (N_Inputs  => 2,
                                             N_Outputs => 1,
                                             Port      => 23671,
                                             Profile   => Basic_Profile,
                                             Server => Server_1_1,
                                             Outputs   =>
                                               (1 => Output_1_1),
                                             Inputs    =>
                                               (1 => Input_1_1, 2 => Input_1_2));

   Test_Data : constant Test_Case_Array :=
                 (1 => (Input  => new String'(String_1),
                        Output => new Config_Data'
                          (1 => new Session_Config'(Session_1_1))
                       ),
                  2 => (Input  => new String'(String_2),
                        Output => new Config_Data'
                          (1 => new Session_Config'(Session_1_1))
                       ));

   type Bad_Case is
      record
         Input  : String_Pt;
         Output : String (1 .. 3);
      end record;

   type Bad_Case_Array is array (Positive range <>) of Bad_Case;

   Test_Bad : constant Bad_Case_Array :=
                (1 => (Input  => new String'(String_Bad_1),
                       Output => "001"),
                 2 => (Input  => new String'(String_Bad_2),
                       Output => "000"),
                 3 => (Input  => new String'(String_Bad_3),
                       Output => "002"),
                 4 => (Input  => new String'(String_Bad_4),
                       Output => "003")
                   );

   function Check_Case (X : Test_Case) return Boolean is
      Result : constant Config_Data := Parse(X.Input.all);
   begin
       -- Dump (Result, "result");
       -- Dump (X.Output.all, "expected");

      if (Result'Length /= X.Output'Length) then
         return False;
      end if;

      for I in Result'Range loop
         if (Result (I).all /= X.Output(I).all) then
            return False;
         end if;
      end loop;

      return True;
   end Check_Case;

   function Check_Bad (X : Bad_Case) return Boolean is
      Junk : Config_Data (1 .. 1);
      Raised : Boolean;
   begin
      Raised := False;
      begin
         Junk := Parse (X.Input.all);
      exception
         when E : Parse_Error =>
            declare
               Msg : String := Exception_Message (E);
            begin
               Raised := Msg (1 .. 3) = X.Output;
               -- Put_Line (Msg);
            end;
      end;

      return Raised;
   end Check_Bad;

   procedure Do_Many_Tests is
      new Do_Suite (Test_Case       => Test_Case,
                    Test_Case_Array => Test_Case_Array,
                    Check           => Check_Case);

   procedure Do_Bad_Cases is
      new Do_Suite (Test_Case       => Bad_Case,
                    Test_Case_Array => Bad_Case_Array,
                    Check           => Check_Bad);

   Reporter : Reporter_Type;

   data : Config_Data := Parse(String_1);
begin

   Dump(data);
--   data := Parse(String_1);


--   Do_Many_Tests (Reporter, Test_Data, "Check parsing");
--   Do_Bad_Cases  (Reporter, Test_Bad, "Check exceptions");
--   Reporter.Final;
end Config_Parser_Test;

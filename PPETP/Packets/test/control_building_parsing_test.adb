with Test_Report;                   use Test_Report;
with Network;                       use Network;
with Byte_Arrays;                   use Byte_Arrays;
with Packets.Protocol.Command;      use Packets.Protocol.Command,
                                      Packets.Protocol;

with Packets.Protocol.Parsing;      use Packets.Protocol.Parsing;
with Packets.Protocol.Building;     use Packets.Protocol.Building;
with Packets.Binary.Network;        use Packets.Binary.Network;
with Profiles.Entangled.Basic;      use Profiles.Entangled.Basic,
                                      Profiles.Entangled, Profiles;
with Profiles.Parameters.Basic;     use Profiles.Parameters;


with PPETP;
with Ada.Text_IO;                   use Ada.Text_IO;
with Auth.Credentials;              use Auth.Credentials;
with Auth.Profiles;                 use Auth.Profiles;

procedure Control_Building_Parsing_Test is

   Cred_1 : Auth_Data := New_Credential (Data    => (1, 2, 3),
                                         Profile => Void_Profile);

   Cred_2 : Auth_Data := New_Credential (Data    => (7, 8, 15, 23),
                                         Profile => Void_Profile);

   Cred_3 : Auth_Data := New_Credential (Data    => (2, 11),
                                         Profile => Void_Profile);

   Addr_1 : Sock_Addr_Type := (Family => Family_Inet,
                               Addr   => Inet_Addr ("128.132.15.64"),
                               Port   => 16#F3e2#); -- 10#62434#

   Addr_2 : Sock_Addr_Type := (Family => Family_Inet,
                               Addr   => Inet_Addr ("192.32.16.8"),
                               Port   => 16#abcd#); -- 10#43981#

   Default_Basic : Parameters_Class_Pt := Parameters.Basic.New_Parameter;

   ---------
   -- "=" --
   ---------

   function "=" (X, Y : Control_Packet) return Boolean is
      Ok : Boolean;
      use type PPETP.Timestamp_Type;
      use type PPETP.Channel_ID;
      use type Auth.Credentials.Auth_Data;
   begin
      if (X.Command /= Y.Command or
          X.Timestamp /= Y.Timestamp) then
         return False;
      end if;

      case X.Command is
         when Hello =>
            Ok := (X.Reply_Port = Y.Reply_Port);
            Ok := Ok and X.Profile = Y.Profile;
         when Acknowledge =>
            Ok := X.ACKed_Number = Y.ACKed_Number;
         when Negative_Acknowledge =>
            Ok := X.NACKed_Number = Y.NACKed_Number;
            Ok := Ok and X.Reason = Y.Reason;
         when Send_Data | Stop  =>
            Ok := X.Target = Y.Target;
            -- Put_Line ("a-" & Boolean'Image (Ok));
            Ok := Ok and X.Channel = Y.Channel;
            Ok := Ok and X.Credentials = Y.Credentials;
            Ok := Ok and X.Credentials_2 = Y.Credentials_2;
         when Nul =>
            Ok := True;
         when Punch =>
            Ok := Ok and X.Who = Y.Who;
            Ok := Ok and X.Server_Cred = Y.Server_Cred;
         when Set_Default =>
            if (X.Default = null) then
               raise Program_Error;
            end if;

            case X.Default.Profile is
               when Basic_Profile =>
                  Ok := True;
            end case;
      end case;

      return Ok;
   end "=";

   type Control_Test_Case is
      record
         Packet    : Control_Packet_Pt;
         Expected  : Byte_Array_Pt;
         Ignore    : Byte_Array_Pt;
      end record;

   type Control_Test_Array is
     array (Positive range <>) of Control_Test_Case;

   Control_Cases : Control_Test_Array :=
     (1 => (Packet   =>
              new Control_Packet'(Timestamp  => 16#12345#,
                                  Peer       => <>,
                                  Command    => Hello,
                                  Reply_Port => 16#9876#,
                                  Profile    => Profiles.Basic_Profile),
            Expected =>
              new Byte_Array'(2#00_1_00000#,
                              16#05#, 16#34#, 16#12#,
                              16#76#, 16#98#, 0, 0),
            Ignore   => null),
      2 => (Packet =>
              new Control_Packet'(Timestamp => 16#72316#,
                                  Peer      => <>,
                                  Command   => Acknowledge,
                                  ACKed_Number => 16#45678#),
            Expected =>
              new Byte_Array'(2#00_1_00010#,
                              16#06#, 16#31#, 16#72#,
                              16#78#, 16#56#, 16#04#, 16#00#),
            Ignore   => null),
      3 => (Packet =>
              new Control_Packet'(Timestamp => 16#1d76c#,
                                  Peer      => <>,
                                  Command   => Negative_Acknowledge,
                                  NACKed_Number => 16#af89b#,
                                  Reason        => Invalid),
            Expected =>
              new Byte_Array'(2#00_1_00011#,
                              16#0c#, 16#76#, 16#1d#,
                              16#9b#, 16#f8#, 16#0a#, 16#00#),
            Ignore   => null),
      4 => (Packet =>
              new Control_Packet'(Timestamp => 16#349ba#,
                                  Peer      => <>,
                                  Command   => Nul),
            Expected =>
            new Byte_Array'(2#00_1_00111#,
                            16#0a#, 16#9b#, 16#34#),
            Ignore   => null),
      5 => (Packet =>
              new Control_Packet'(Timestamp     => 16#17bc9#,
                                  Peer          => <>,
                                  Command       => Send_Data,
                                  Target        => Addr_1,
                                  Channel       => 16#B#,
                                  Credentials   => Cred_1,
                                  Credentials_2 => No_Auth),
            Expected =>
            new Byte_Array'(2#00_1_00100#,
                            16#09#, 16#bc#, 16#17#,
                            16#00#, 16#2c#, 16#E2#, 16#F3#,
                            16#80#, 16#84#, 16#0f#, 16#40#,
                            16#00#, 16#03#, 16#00#, 16#00#,
                            16#01#, 16#02#, 16#03#),
            Ignore => null),
      6 => (Packet =>
              new Control_Packet'(Timestamp     => 16#3011f#,
                                  Peer          => <>,
                                  Command       => Receive_Data,
                                  Target        => Addr_2,
                                  Channel       => 16#4#,
                                  Credentials   => Cred_2,
                                  Credentials_2 => Cred_3),
            Expected =>
            new Byte_Array'(2#00_1_00101#,
                            16#0f#, 16#11#, 16#30#,
                            16#00#, 16#10#, 16#cd#, 16#AB#,
                            16#c0#, 16#20#, 16#10#, 16#08#,
                            16#00#, 16#04#, 16#00#, 16#00#,
                            16#07#, 16#08#, 16#0f#, 16#17#,
                            16#00#, 16#02#, 16#00#, 16#00#,
                            16#02#, 16#0b#),
            Ignore => null),
      7 => (Packet =>
              new Control_Packet'(Timestamp     => 16#17bc9#,
                                  Peer          => <>,
                                  Command       => Stop,
                                  Target        => Addr_1,
                                  Channel       => 16#B#,
                                  Credentials   => Cred_1,
                                  Credentials_2 => No_Auth),
            Expected =>
            new Byte_Array'(2#00_1_00110#,
                            16#09#, 16#bc#, 16#17#,
                            16#00#, 16#2c#, 16#E2#, 16#F3#,
                            16#80#, 16#84#, 16#0f#, 16#40#,
                            16#00#, 16#03#, 16#00#, 16#00#,
                            16#01#, 16#02#, 16#03#),
            Ignore => null),
      8 => (Packet =>
              new Control_Packet'(Timestamp     => 16#320ac#,
                                  Peer          => <>,
                                  Command       => Set_Default,
                                  Default       => Default_Basic),
            Expected =>
            new Byte_Array'(2#00_1_00001#,
                            16#0c#, 16#0a#, 16#32#),
            Ignore => null)

      );

   function Need_Profile (X : Control_Packet) return Boolean is
   begin
      return (X.Command = Hello or X.Command = Set_Default);
   end Need_Profile;

   function Get_Profile (X : Control_Packet) return Profile_Type is
   begin
      if (X.Command = Hello) then
         return X.Profile;
      elsif (X.Command = Set_Default) then
         return X.Default.Profile;
      else
         raise Program_Error;
      end if;
   end Get_Profile;

   -- This function converts X.Packet into a Network_Packet
   -- and checks that the result is equal to X.Expected
   function Check_Control_Case (X : Control_Test_Case)
                             return Boolean is
      Result  : Network_Packet;

      Builder : Packet_Builder;
      use type Byte_Arrays.Byte;
      use type Byte_Arrays.Byte_Array_Offset;
   begin
      if (Need_Profile (X.Packet.all)) then
         Builder.Set_Profile (Get_Profile(X.Packet.all));
      end if;

      Result := Builder.Make_Packet (Source => X.Packet.all);
      --Dump (Result.Buffer,  "got     ");
      --Dump (X.Expected.all, "expected");
      if (X.Ignore = null) then
         return Result.Buffer = X.Expected.all;
      else
         -- Here Ignore is not null.  We check the equality of
         -- X.Expected(I) and the I-th byte of the result, only
         -- if X.Ignore(I)=0

         --Put_Line (Byte_Array_Offset'Image (X.Expected'First));
         --Put_Line (Byte_Array_Offset'Image (X.Expected'Last));
         for I in X.Expected'Range loop
            if (Result.Get (I - X.Expected'First + 1) /= X.Expected (I)
                and X.Ignore (I) = 0) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Check_Control_Case;

   -- This function converts X.Packet to a Network_Packet,
   -- converts back the result to a Control_Packet and check
   -- that the resulting packet is equal to X.Packet.
   function Control_Case_Back_And_Forth (X : Control_Test_Case)
                             return Boolean is
      use type Byte_Arrays.Byte;
      use type Byte_Arrays.Byte_Array_Offset;
      use type PPETP.Timestamp_Type;

      Source  : Control_Packet := X.Packet.all;
      Net_Pkt : Network_Packet;

      Builder : Packet_Builder;
      Parser  : Packet_Parser;

   begin
      if (Need_Profile(Source)) then
         Builder.Set_Profile (Get_Profile(Source));
         Parser.Set_Profile  (Get_Profile(Source));
      end if;


      Net_Pkt := Builder.Make_Packet (Source => Source);
      --Dump (Net_Pkt.Buffer, "B&F");
      declare
         Tmp : Protocol_Packet'Class := Parser.Parse_Packet (Net_Pkt);
      begin
         if (Tmp not in Control_Packet) then
            return False;
         end if;

         declare
            Result : Control_Packet := Control_Packet (Tmp);
         begin
            --Put("[--Source--]"); Print(Result);
            --Put("[--Result--]"); Print(Result);
            return Result = Source;
         end;
      end;

      -- Put_Line (PPETP.Timestamp_Type'Image (Result.Timestamp));
      -- Put_Line (Profile_Type'Image (Result.Profile));
      -- Dump (Basic_Ent_Pt (Result.Payload).Bin_Data.all);

      -- Ok := Result.Timestamp = Source.Timestamp;
      -- Ok := Ok and Result.Profile = Source.Profile;
      -- Ok := Ok and Basic_Ent_Pt (Result.Payload).Bin_Data.all = X.Data.all;
   end Control_Case_Back_And_Forth;

   function Check_Exception return Boolean is
      Builder : Packet_Builder;
      Ignored : Network_Packet;
      X       : Control_Test_Case := Control_Cases (1);

      Source_1 : Control_Packet := (Timestamp  => 16#12345#,
                                    Peer       => <>,
                                    Command    => Hello,
                                    Reply_Port => 16#9876#,
                                    Profile    => Profiles.Basic_Profile);

      Source_2 : Control_Packet := (Timestamp  => 16#12345#,
                                    Peer       => <>,
                                    Command    => Set_Default,
                                    Default    => null);
   begin
      declare
         Builder : Packet_Builder;
      begin
         -- Source_1 is a Hello packet and does not require a profile
         -- An exception should **not** be raised
         Ignored := Builder.Make_Packet (Source_1);
      exception
         when Profile_Required =>
            return False;
      end;

      declare
         Builder : Packet_Builder;
      begin
         -- Source_2 is a Set_Default packet and requires a profile
         -- An exception should be raised
         Ignored := Builder.Make_Packet (Source_2);
      exception
         when Profile_Required =>
            return True;
      end;

      -- If I am here, the exception for Source_2 was not raised
      return False;
   end Check_Exception;

   Reporter : Reporter_Type;

   procedure Check_Many_Control_Cases is
     new Do_Suite (Test_Case       => Control_Test_Case,
                   Test_Case_Array => Control_Test_Array,
                   Check           => Check_Control_Case);

   procedure Many_Control_Back_And_Forth is
     new Do_Suite (Test_Case       => Control_Test_Case,
                   Test_Case_Array => Control_Test_Array,
                   Check           => Control_Case_Back_And_Forth);
begin
   -- Reporter.Be_Verbose;

   Reporter.New_Suite ("Profile_Required Exception");
   Reporter.New_Result (Check_Exception);

   --Reporter.Be_Verbose;
   Check_Many_Control_Cases (Reporter, Control_Cases, "Building basic profile");

   -- Reporter.Be_Verbose;
   Many_Control_Back_And_Forth (Reporter, Control_Cases, "Back & Forth");

   Reporter.Final;
end Control_Building_Parsing_Test;

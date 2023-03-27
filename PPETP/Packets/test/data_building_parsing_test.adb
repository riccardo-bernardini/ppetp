with Test_Report;                   use Test_Report;
with Network;                       use Network;
with Byte_Arrays;                   use Byte_Arrays;
with Packets.Protocol.Command;      use Packets.Protocol;

with Packets.Protocol.Data;         use Packets.Protocol.Data;
with Packets.Protocol.Parsing;      use Packets.Protocol.Parsing;
with Packets.Protocol.Building;     use Packets.Protocol.Building;
with Packets.Binary.Network;        use Packets.Binary.Network;
with Profiles.Entangled.Basic;      use Profiles.Entangled, Profiles;
                                    use Profiles.Entangled.Basic;
with PPETP;
with Ada.Text_IO;                   use Ada.Text_IO;

procedure Data_Building_Parsing_Test is

   type Data_Test_Case is
      record
         Profile   : Profiles.Profile_Type;
         Data      : Byte_Array_Pt;
         Timestamp : PPETP.Timestamp_Type;
         Expected  : Byte_Array_Pt;
         Ignore    : Byte_Array_Pt;
      end record;

   type Data_Test_Array is
     array (Positive range <>) of Data_Test_Case;

   Data_Cases : Data_Test_Array :=
                  (1 => (Profile   => Profiles.Basic_Profile,
                         Data      => new Byte_Array'(1,2,3),
                         Timestamp => 16#12345#,
                         Expected  =>
                         new Byte_Array'
                           (8, 16#05#, 16#34#, 16#12#, 1, 2, 3, 1),
                         Ignore    => null),
                   2 => (Profile   => Profiles.Basic_Profile,
                         Data      => new Byte_Array'(6,7),
                         Timestamp => 16#bcdef#,
                         Expected  =>
                         new Byte_Array'
                           (8, 16#0f#, 16#de#, 16#bc#, 6, 7, 0, 2),
                         Ignore    =>
                         new Byte_Array'
                           (0,  0,      0,      0,     0,  0, 1, 0)),
                   3 => (Profile   => Profiles.Basic_Profile,
                         Data      => new Byte_Array'(11,12,13,14),
                         Timestamp => 16#6789a#,
                         Expected  =>
                         new Byte_Array'
                           (0, 16#0a#, 16#89#, 16#67#, 11, 12, 13, 14),
                         Ignore    => null)
                  );

   function Check_Data_Case (X : Data_Test_Case)
                             return Boolean is
      Payload : Basic_Ent_Pt :=
                  new Basic_Ent'(Root_Profile_Handler with
                                 Bin_Data => new Byte_Array'(X.Data.all));

      Source : Data_Packet := (Profile   => X.Profile,
                               Payload   => Entangled_Payload_Pt (Payload),
                               Timestamp => X.Timestamp,
                               Peer      => <>);
      Result  : Network_Packet;

      Builder : Packet_Builder;
      use type Byte_Arrays.Byte;
      use type Byte_Arrays.Byte_Array_Offset;
   begin
      Builder.Set_Profile (X.Profile);
      Result := Builder.Make_Packet (Source => Source);
      -- Dump (Result.Buffer, "got");
      -- Dump (X.Expected.all, "expected");
      if (X.Ignore = null) then
         return Result.Buffer = X.Expected.all;
      else
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
   end Check_Data_Case;

   function Data_Case_Back_And_Forth (X : Data_Test_Case)
                             return Boolean is
      Payload : Basic_Ent_Pt :=
                  new Basic_Ent'(Root_Profile_Handler with
                                 Bin_Data => new Byte_Array'(X.Data.all));

      Source : Data_Packet := (Profile   => X.Profile,
                               Payload   => Entangled_Payload_Pt (Payload),
                               Timestamp => X.Timestamp,
                               Peer      => <>);
      Net_Pkt : Network_Packet;
      Result  : Data_Packet;

      Builder : Packet_Builder;
      Parser  : Packet_Parser;
      use type Byte_Arrays.Byte;
      use type Byte_Arrays.Byte_Array_Offset;
      use type PPETP.Timestamp_Type;

      Ok : Boolean;
   begin
      Builder.Set_Profile (X.Profile);
      Parser.Set_Profile (X.Profile);
      Net_Pkt := Builder.Make_Packet (Source => Source);

      declare
         Tmp : Protocol_Packet'Class := Parser.Parse_Packet (Net_Pkt);
      begin
         if (Tmp not in Data_Packet) then
            return False;
         end if;

         Result := Data_Packet (Tmp);
      end;

      -- Put_Line (PPETP.Timestamp_Type'Image (Result.Timestamp));
      -- Put_Line (Profile_Type'Image (Result.Profile));
      -- Dump (Basic_Ent_Pt (Result.Payload).Bin_Data.all);

      Ok := Result.Timestamp = Source.Timestamp;
      Ok := Ok and Result.Profile = Source.Profile;
      Ok := Ok and Basic_Ent_Pt (Result.Payload).Bin_Data.all = X.Data.all;
      return Ok;
   end Data_Case_Back_And_Forth;

   Reporter : Reporter_Type;
   Exception_Raised : Boolean;

   procedure Check_Many_Data_Cases is
     new Do_Suite (Test_Case       => Data_Test_Case,
                   Test_Case_Array => Data_Test_Array,
                   Check           => Check_Data_Case);

   procedure Many_Data_Back_And_Forth is
     new Do_Suite (Test_Case       => Data_Test_Case,
                   Test_Case_Array => Data_Test_Array,
                   Check           => Data_Case_Back_And_Forth);
begin
   -- Reporter.Be_Verbose;

   Reporter.New_Suite ("Profile_Required Exception");

   Exception_Raised := False;
   declare
      Builder : Packet_Builder;
      Ignored : Network_Packet;
      X       : Data_Test_Case := Data_Cases (1);

      Payload : Basic_Ent_Pt :=
                  new Basic_Ent'(Root_Profile_Handler with
                                 Bin_Data => new Byte_Array'(X.Data.all));

      Source : Data_Packet := (Profile   => X.Profile,
                               Payload   => Entangled_Payload_Pt (Payload),
                               Timestamp => X.Timestamp,
                               Peer      => <>);
   begin
      Ignored := Builder.Make_Packet (Source);
   exception
      when Profile_Required =>
         Exception_Raised := True;
   end;
   Reporter.New_Result(Ok => Exception_Raised);

   Check_Many_Data_Cases (Reporter, Data_Cases, "Building basic profile");

   Many_Data_Back_And_Forth(Reporter, Data_Cases, "Back & Forth");

   Reporter.Final;
end Data_Building_Parsing_Test;

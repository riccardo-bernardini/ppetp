with PPETP;         use PPETP;
with Packets;       use Packets;
with Ada.Streams;   use Ada.Streams;
with Text_Io;       use Text_Io;


-- with Profiles.Vandermonde;
-- with Profiles.Basic;
use  Profiles;

with Network;

procedure Test_Packets is
   package Timestamp_Io is new Text_Io.Modular_Io(Timestamp_Type);

   package St_El_Io     is new Text_Io.Modular_Io(Stream_element);

   procedure Dump(X: Stream_Element_Array) is

   begin
      for I in X'Range loop
         Put (Stream_Element_Offset'Image(I) & " -> ");
         St_El_Io.Put (X(I), Base => 2, Width => 8);
         Put (", ");
         St_El_Io.Put (X(I), Base => 16);
         Put (", ");
         St_El_Io.Put (X(I));
         New_Line;
      end loop;
   end Dump;

   function New_Payload
     return Data_Class_Pt is
      Result : Medusa.Payload := new Stream_Element_Array(1..10);
   begin
      for I in Result'Range loop
         Result(I) := Stream_Element(I);
      end loop;

      return Result;
   end New_Payload;

   type Parsed_Class_Access is access Parse_Packet'Class;

   type Test_Case is
      record
         Def : Medusa.Details_Type;
         Pkt : Parsed_Class_Access;
      end record;

   type Test_Array is
     array(Positive range <>) of Test_Case;


   Default : Medusa.Details_type := (Galois_Field     => GF_32,
                                     Reduction_Factor => 12,
                                     Reduction_Vector => 16#ABCD_0099#);

   Def_Peer : Network.Sock_Addr_Type := (Family => Family_Inet,
                                         Addr   => Inet_Addr("127.0.1.2"),
                                         Port   => 54321);

   Case_Array : Data_Test_Array :=
     ( 1=> (Def => Default,
            Pkt => new Data_Packet'(Timestamp => 16#ABCDE#,
                                    Peer      => Def_Peer,
                                    Profile   => Basic,
                                    Payload   => New_Payload) ) );

   function Check (Trial : Test_Case) return Boolean is
      Bytes : Raw_Packet_Pt :=
        Make_Packet(Source  => Trial.Pkt.all,
                    Default => Trial.Def);
   begin
      return Trial.Pkt.all = Parse_Packet(Packet  => Bytes,
                                          Peer    => Trial.Peer,
                                          Default => Trial.Def);
   end Check;

   procedure Do_Many_Tests is
      new Do_Suite (Test_Case       => Test_Case,
                    Test_Case_Array => Test_Array,
                    Check           => Check);

   Reporter : Reporter_Type;
begin
   Reporter.Do_Many_Tests (Case_Array);
   Reporter.Final;
end Test_Packets;

--
--   if (not Ok) then
--      Put_Line("something wrong");
--   elsif (Recovered /= Original) then
--      Put_Line("Bad reconstruction");
--
--      Print(Original);
--      Print(Recovered);
--   else
--      Put_Line("OK");
--   end if;
----   procedure Print (A : Parsed_Packet'Class) is
--   begin
--      Put_Line ("GF  = " & GF_Name'Image(A.Details.Galois_Field));
--      Put_Line ("Fac = " & Reduction_Factor_Type'Image(A.Details.Reduction_Factor));
--      Put_Line ("Vec = " & RV_Index'Image(A.Details.Reduction_Vector));
--
--      Put ("T   = ");
--      Timestamp_Io.Put (Item => A.Timestamp, Base => 2);
--      Put (", ");
--      Timestamp_Io.Put (Item => A.Timestamp);
--      New_Line;
--
--      Put_Line ("Pad = " & Boolean'Image(A.Padding));
--      Put_Line ("Payload:");
--
--      for I in A.Payload'Range loop
--         Put_Line (Stream_Element_Offset'Image(I)
--                   & " -> "
--                   & Stream_Element'Image(A.Payload(I)));
--      end loop;
--
--   end Print;
-- function "="(A, B : Parsed_Packet'Class)
--                return Boolean is
--    begin
--       if (A.Details     /= B.Details   or else
--           A.Timestamp   /= B.Timestamp or else
--           A.Padding     /= B.Padding   or else
--           A.Payload'Length /= B.Payload'Length) then
--          return False;
--       else
--          for I in A.Payload'Range loop
--             if (A.Payload(I) /= B.Payload(I-A.Payload'First+B.Payload'First)) then
--                return False;
--             end if;
--          end loop;
--
--          return True;
--       end if;
--    end "=";

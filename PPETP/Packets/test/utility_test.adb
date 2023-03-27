with Test_Report;                   use Test_Report;
with Packets.Protocol.Utilities;    use Packets.Protocol.Utilities;
with PPETP;                         use PPETP;
with Byte_Arrays;                   use Byte_Arrays;
with Ada.Text_Io;                   use Ada.Text_Io;

procedure Utility_Test is
   procedure Dump (X : Profile_Timestamp_Buffer; S : String := "") is
   begin
      Put(S & " [");
      for I in X'Range loop
         Put (Byte'Image (X (I)));
      end loop;
      Put_Line("]");
   end Dump;
   Buffer   : Profile_Timestamp_Buffer;

   Reporter : Reporter_Type;
   use type Byte;
begin
   -- Reporter.Be_Verbose;
   Reporter.New_Suite("Timestamp conversion");
   declare
      T : PPETP.Timestamp_Type := 16#12345#;
      Test_Flag : Bit_Field_4 := 16#D#;
   begin
      Buffer := (others => 0);
      To_Buffer (Buffer, T);
      Reporter.New_Result (T = To_Timestamp(Buffer));

      Buffer(1) := 16#77#;
      To_Buffer(Buffer, T);
      Reporter.New_Result (T = To_Timestamp (Buffer));

      Reporter.New_Result (Buffer (1) = Byte (16#70# + (T and 16#0F#)));

      Set_Flags (Buffer, Test_Flag);
      Reporter.New_Result (Get_Flags(Buffer) = Test_Flag);
   end;

   declare
      Buf : Profile_Timestamp_Buffer := (16#32#, 16#98#, 16#FA#);
      T0  : Timestamp_Type   := 16#FA982#;
   begin
      Buffer (1) := 16#38#;
      To_Buffer(Buffer, T0);
      Reporter.New_Result (Buf = Buffer);
      Reporter.New_Result (T0 = To_Timestamp(Buf));
   end;

   Reporter.Final;
end Utility_Test;

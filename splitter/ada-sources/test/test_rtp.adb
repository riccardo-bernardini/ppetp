--
with Ada.Streams;                             use Ada.Streams;
with Test_Report;                             use Test_Report;
with Splitter_Lib.RTP;                        use Splitter_Lib;
procedure Test_Rtp is
   -- Initial part of an RTP_packet
   Packet : Stream_Element_Array :=
              (16#80#, 16#88#, 16#E6#, 16#Fd#,
               16#00#, 16#00#, 16#00#, 16#F0#,
               16#DE#, 16#E0#, 16#Ee#, 16#8f#,
               16#D5#, 16#D5#, 16#D5#, 16#D5#);

   Too_Short : Stream_Element_Array :=
                 (16#80#, 16#88#, 16#E6#, 16#Fd#,
                  16#00#, 16#00#, 16#00#, 16#F0#);

   use type RTP.SSRC_Type;

   Reporter : Reporter_Type;
   SSRC     : RTP.SSRC_Type;
   Ok       : Boolean;
begin
   begin
      Ok := False;
      SSRC := RTP.Get_SSRC (Too_Short);
   exception
      when RTP.Invalid_RTP_Packet =>
         Ok := True;
   end;

   New_Result (Reporter, Ok);
   SSRC := RTP.Get_SSRC (Packet);
   New_Result (Reporter, SSRC = 16#8FEE_E0DE#);
   Final(Reporter);
end Test_Rtp;

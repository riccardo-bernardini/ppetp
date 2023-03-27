--
with Ada.Streams;                      use Ada.Streams;
with Ada.Unchecked_Conversion;

package body Splitter_Lib.RTP is

   --------------
   -- Get_SSRC --
   --------------

   function Get_SSRC
     (Packet : Stream_Element_Array)
      return SSRC_Type
   is
      First : constant Stream_Element_Offset := Packet'First;

      type SSRC_Field is new Stream_Element_Array (1..4);
      function To_SSRC is
        new Ada.Unchecked_Conversion (Source => SSRC_Field,
                                      Target => SSRC_Type);

   begin
      if Packet'Length < 12 then
         raise Invalid_RTP_Packet;
      end if;

      return To_SSRC (SSRC_Field(Packet (First + 8 .. First + 11)));
   end Get_SSRC;

end Splitter_Lib.RTP;

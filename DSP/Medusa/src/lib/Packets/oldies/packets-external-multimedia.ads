package Packets.External.Multimedia is
   type Multimedia_Packet is new Byte_Packet with record
      Timestamp : Timestamp_Type;
   end record;
end Packets.External.Multimedia;

package body Generic_Oxidizers is

   ----------
   -- Init --
   ----------

   procedure Init (Oxidizer   : in out Oxidizer_Type;
                   LL_Recover : in     Low_Level_Recoverer_Type)
   is
   begin
      Oxidizer.LL_Recover := LL_Recover;
      Oxidizer.N_Packets  := Min_Packet_Number(LL_Recover);

      Packet_Table_Pkg.Init(Oxidizer.Received, Oxidizer.N_Packets);
   end Init;

   --------------------
   -- Receive_Packet --
   --------------------

   procedure Receive_Packet
     (Oxidizer : in out Generic_Oxidizer;
      Packet   : in     Packet_Type;
      Complete :    out Boolean)
   is
      Inserted : Positive;
   begin
      pragma Assert(Packet.Portion = Oxidizer.My_Portion);

      Packet_Table_Pkg.Insert(Table     => Oxidizer.Received,
                              Index     => Packet.Timestamp,
                              Packet    => Packet,
                              Inserted  => Inserted);

      Complete := Inserted >= Oxidizer.N_Packets;
   end Receive_Packet;

   -----------------
   -- Packet_Lost --
   -----------------

   procedure Packet_Lost
     (Oxidizer  : in out Generic_Oxidizer;
      Timestamp : in     Timestamp_Type)
   is
   begin
      Packet_Table_Pkg.Remove(Table => Oxidizer.Received,
                              Index => Timestamp);
   end Packet_Lost;

   procedure Recover (Oxidizer  : in out Oxidizer_Type;
                      Timestamp : in     Timestamp_Type;
                      Recovered :    out Complete_Type;
                      Success   :    out Boolean) is
      Reduced_Data : Reduced_Type_Array :=
        Get(Oxidizer.Received, Timestamp);
   begin
      Recover(LL_Recover => Oxidizer.LL_Recover,
              Reduced    => Reduced_Data,
              Recovered  => Recovered,
              Success    => Success);
   end Recover;

end Generic_Oxidizers;

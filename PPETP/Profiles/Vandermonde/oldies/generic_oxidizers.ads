-- #### ROBA VECCHIA ####
-- with Timestamps;
-- use  Timestamps;
--
-- with Generic_Packet_Map;

generic
   -- Type of the reduced packet and of an array of reduced packets
   type Reduced_Type is
     new Basic_Reduced_Type with private;

   type Reduced_Type_Array is
     array (Positive range <>) of Reduced_Type;

   -- Type of the complete packet

   type Complete_Type is
     new Basic_Complete_Type with private;

   -- Object used as callback to carry out the actual
   -- reconstruction

   type Basic_Recoverer_Type is private;

   with procedure Recover (LL_Recover : in out Basic_Recoverer_Type;
                           Reduced    : in     Reduced_Type_Array;
                           Recovered  :    out Complete_Type;
                           Success    :    out Boolean) is <>;

   with function Min_Packet_Number (LL_Recover : Basic_Recoverer_Type)
     return Natural is <>;

package Generic_Oxidizers is
   type Oxidizer_Type is private;

   procedure Init (Oxidizer   : in out Oxidizer_Type;
                   LL_Recover : in     Low_Level_Recoverer_Type);

   procedure Receive (Oxidizer : in out Oxidizer_Type;
                      Packet   : in     Reduced_Type;
                      Complete :    out Boolean);

   procedure Recover (Oxidizer  : in out Oxidizer_Type;
                      Timestamp : in     Timestamp_Type;
                      Recovered :    out Complete_Type;
                      Success   :    out Boolean);

   procedure Emergency_Recover (Oxidizer  : in out Oxidizer_Type;
                                Timestamp : in     Timestamp_Type;
                                Recovered :    out Reduced_Type;
                                Success   :    out Boolean);

   procedure Lost (Oxidizer  : in out Oxidizer_Type;
                   Timestamp : in     Timestamp_Type);

private
   package Packet_Table_Pkg is
      new Generic_Packet_Map (Packet_Type => Reduced_Type,
                              Index_Type  => Crumb_Timestamp);

   type Oxidizer_Type is record
      Received   : Packet_Table_Pkg.Packet_Table;
      LL_Recover : Low_Level_Recoverer_Type;
      N_Packet   : Natural;
   end record;
end Generic_Oxidizers;

with Text_Io; use Text_Io;

--
-- #### ROBA VECCHIA ####
--
-- with Types.GF256.Matrices;
-- use  Types.GF256.Matrices;
-- use  Types.GF256.Matrices.GFM;
-- use  Types.GF256;
-- use  Types.GF256.GF;
-- use  type Types.GF256.Matrices.GF256_Matrix;
-- with Types.Bytedef;
-- use  Types.Bytedef;
-- with Generic_Oxidizer_Maps;


package body Binary_Oxidizers is
   use Binary_Oxi_Pkg;
   use Binary_Oxi_Pkg.Packet_Table_Pkg;

   subtype Idx_Type is Byte;
   type Idx_Array is array (Positive range <>) of Idx_type;

   package Matrix_Maps is
      new Generic_Oxidizer_Maps(Element => GF256_Matrix,
                                Index   => Idx_type,
                                Index_array => Idx_array);


   Matrix_Cache : Matrix_Maps.Matrix_Map;

   -- ----------
   -- -- Init --
   -- ----------
   --
   -- procedure Init (Oxidizer  : in out Binary_Oxidizer;
   --                 N_Packets : in     Positive;
   --                 Portion   : in     Portion_Type) is
   -- begin
   --    Init(Oxidizer.Received, N_Packets);
   --    Oxidizer.N_Packets  := N_Packets;
   --    Oxidizer.My_Portion := Portion;
   -- end Init;
   --
   --
   -- --------------------
   -- -- Receive_Packet --
   -- --------------------
   --
   -- procedure Receive_Packet (Oxidizer : in out Binary_Oxidizer;
   --                           Packet   : in     Reduced_Binary_Packet;
   --                           Complete :    out Boolean)
   -- is
   --    Inserted : Positive;
   -- begin
   --    Insert(Table     => Oxidizer.Received,
   --           Timestamp => Packet.Timestamp,
   --           Packet    => Packet,
   --           Inserted  => Inserted);
   --
   --    Complete := Inserted = Oxidizer.N_Packets;
   -- end Receive_Packet;
   --
   -- -----------------
   -- -- Packet_Lost --
   -- -----------------
   --
   -- procedure Packet_Lost (Oxidizer  : in out Binary_Oxidizer;
   --                        Timestamp : in     Timestamp_Type) is
   -- begin
   --    Remove(Table     => Oxidizer.Received;
   --           Timestamp => Timestamp);
   -- end Packet_Lost;



   procedure Fill_Matrix (Pkts   : in     Packet_Vector;
                          Result : in out GF256_Matrix)
   is
      Ncol : Positive := Pkts(1).Payload'Length;
      Nrow : Positive := Pkts'Length;
   begin
      GFM.Resize(Result, Nrow, Ncol);
      for I in Pkts'Range loop
         if (Pkts(I).Payload'Length /= Ncol) then
            raise Constraint_Error;
         end if;
      end loop;

      for Row in Pkts'Range loop
         for Col in 1..Ncol loop
            GFM.Set(Result, Row, Col, Byte_To_GF256(Pkts(Row).Payload(Col)));
         end loop;
      end loop;
   end Fill_Matrix;

   procedure Make_Packet (Result    :    out Binary_Packet;
                          Data      : in     GF256_Matrix;
                          Timestamp : in     Timestamp_Type;
                          Portion   : in     Portion_Type;
                          Padding   : in     Natural)
   is
   begin
      Result := (Timestamp => Timestamp,
                 Portion   => Portion,
                 Payload   => new Binary_Payload(1..Size(Data)-Padding));

      for I in Result.Payload'Range loop
         Result.Payload(I) := Byte(To_Int(Get(Data, I)));
      end loop;
   end Make_Packet;

   function Restoring_Matrix(Packets : Packet_Vector)
                                return GF256_Matrix is
      Indexes : Idx_array (Packets'Range);
      Mtx : GF256_Matrix;
      Found : Boolean;
   begin
      for I in Indexes'Range loop
         Indexes(I) := Packets(I).Vector;
      end loop;

      -- Check if we already computed the requested matrix
      Matrix_Maps.Get(Map => Matrix_Cache,
                      Key => Indexes,
                      Found => Found,
                      Matrix => Mtx);

      if (not Found) then
         -- The matrix is not in the cache: compute it
         declare
            Bases : GF256_Array(Packets'Range);
         begin
            for I in Bases'Range loop
               Bases(I) := Byte_To_Gf256(Packets(I).Vector);
            end loop;

            GFM.Wandermonde(Result => Mtx,
                            Ncol   => Packets'Length,
                            Bases  => Bases);

            Mtx := Inv(Mtx);

            -- Add the matrix to the cache
            Matrix_Maps.Insert(Map => Matrix_Cache,
                               Key => Indexes,
                               New_Item => Mtx);
         end;
      end if;

      -- Here Mtx will always contain the desired matrix
      return Mtx;
   end Restoring_Matrix;

   procedure Recover (Oxidizer  : in out Binary_Oxidizer;
                      Timestamp : in     Timestamp_Type;
                      Recovered :    out Boolean;
                      Packet    :    out Binary_Packet)
   is
      Pkts : Packet_Vector := Get(Oxidizer.Received, Timestamp);

      Mtx, Data_Mtx : GF256_Matrix;
   begin
      if (Pkts'Length < Oxidizer.N_Packets) then
         Recovered := False;
         return;
      end if;

      Fill_Matrix(Result => Data_Mtx,
                  Pkts   => Pkts);

      Make_packet(Result => Packet,
                  Data   => Restoring_Matrix(Pkts)*Data_Mtx,
                  Timestamp => Timestamp,
                  Portion   => Pkts(1).Portion,
                  Padding   => Pkts(1).Padding);

      Recovered := True;
   end Gimme_Packet;

end Binary_Oxidizers;

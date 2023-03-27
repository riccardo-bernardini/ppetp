with Text_Io; use Text_Io;

with Types.Float_Matrices;
use  Types.Float_Matrices;
use  Types.Float_Matrices.Float_Matrices_Pkg;

with Index_Ranges;
use  Index_Ranges;

with Types.Bytedef;
use  Types.Bytedef;

with Generic_Oxidizer_Maps;

with Reduction_Vectors;
use  Reduction_Vectors;

package body Analog_Oxidizers is
   use Analog_Oxi_Pkg;
   use Analog_Oxi_Pkg.Packet_Table_Pkg;

   subtype Idx_Type is Natural;
   type Idx_Array is array (Positive range <>) of Idx_type;

   package Matrix_Maps is
      new Generic_Oxidizer_Maps(Element => Float_Matrix,
                                Index   => Idx_type,
                                Index_array => Idx_array);

   Matrix_Cache : Matrix_Maps.Matrix_Map;

   -- ----------
   -- -- Init --
   -- ----------
   --
   -- procedure Init (Oxidizer  : in out Analog_Oxidizer;
   --                 N_Packets : in     Positive;
   --                 Portion   : in     Portion_Type) is
   -- begin
   --    Init(Oxidizer.Received, N_Packets);
   --    Oxidizer.N_Packets  := N_Packets;
   --    Oxidizer.My_Portion := Portion;
   -- -- end Init;
   --
   -- --------------------
   -- -- Receive_Packet --
   -- --------------------
   --
   -- procedure Receive_Packet
   --   (Oxidizer : in out Analog_Oxidizer;
   --    Packet   : in     Reduced_Analog_Packet;
   --    Complete :    out Boolean)
   -- is
   --    Inserted : Positive;
   -- begin
   --    pragma Assert(Packet.Portion = Oxidizer.My_Portion);
   --
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
   -- procedure Packet_Lost (Oxidizer  : in out Analog_Oxidizer;
   --                        Timestamp : in     Timestamp_Type) is
   -- begin
   --    Remove(Table     => Oxidizer.Received;
   --           Timestamp => Timestamp);
   -- end Packet_Lost;



   function Restoring_Matrix(Packets : Packet_Vector)
                                return Float_Matrix is
      Indexes : Idx_array (Packets'Range);
      Mtx   : Float_Matrix;
      Found : Boolean;
   begin
      for I in Indexes'Range loop
         Indexes(I) := Packets(I).Vector;
      end loop;

      -- Check we already computed the requested matrix
      Matrix_Maps.Get(Map => Matrix_Cache,
                      Key => Indexes,
                      Found => Found,
                      Matrix => Mtx);

      if (not Found) then
         -- The matrix is not in the cache: compute it
         declare
            Block_Size : Positive := Packets(1).Block_Size;
         begin
            Resize(Mtx, Packets'Length, Block_Size);

            for Reducer_Idx in Indexes'Range loop
               Put_Submatrix(Dst => Mtx,
                             Row => Reducer_Idx,
                             Col => Full_Range,
                             Src => Get_Reduction_Vector(Indexes(Reducer_Idx),
                                                         Block_Size));
            end loop;

            if (Size_Row(Mtx) = Size_Col(Mtx)) then
               -- Matrix is a square: use its inverse
               Mtx := Inv(Mtx);
            else
               -- Matrix is not square: use its pseudo-inverse
               Mtx := Inv(Transpose(Mtx)*Mtx)*Transpose(Mtx);
            end if;

            -- Add the matrix to the cache
            Matrix_Maps.Insert(Map => Matrix_Cache,
                               Key => Indexes,
                               New_Item => Mtx);
         end;
      end if;

      -- Here Mtx will always contain the desired matrix
      return Mtx;
   end Restoring_Matrix;


   ------------------
   -- Gimme_Packet --
   ------------------

   procedure Fill_Matrix (Pkts   : in     Packet_Vector;
                          Result : in out Float_Matrix)
   is
      Ncol : Positive := Pkts(1).Payload'Length;
      Nrow : Positive := Pkts'Length;
   begin
      Resize(Result, Nrow, Ncol);

      for I in Pkts'Range loop
         if (Pkts(I).Payload'Length /= Ncol) then
            raise Constraint_Error;
         end if;
      end loop;

      for Row in Pkts'Range loop
         for Col in 1..Ncol loop
            Set(Result, Row, Col, Pkts(Row).Payload(Col));
         end loop;
      end loop;
   end Fill_Matrix;

   procedure Make_Packet (Result    :    out Analog_Packet;
                          Data      : in     Float_Matrix;
                          Timestamp : in     Timestamp_Type;
                          Portion   : in     Portion_Type;
                          Padding   : in     Natural)
   is
   begin
      Result := (Timestamp => Timestamp,
                 Portion   => Portion,
                 Payload   => new Analog_Payload(1..Size(Data)-Padding));

      for I in Result.Payload'Range loop
         Result.Payload(I) := Get(Data, I);
      end loop;
   end Make_Packet;


   procedure Gimme_Packet
     (Oxidizer  : in out Analog_Oxidizer;
      Timestamp : in      Timestamp_Type;
      Recovered :     out Boolean;
      Packet    :     out Analog_Packet)
   is
      Pkts : Packet_Vector := Get(Oxidizer.Received, Timestamp);

      Mtx, Data_Mtx : Float_Matrix;
   begin
      if (Pkts'Length = 0 or else Pkts'Length < Pkts(1).Block_Size) then
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

end Analog_Oxidizers;

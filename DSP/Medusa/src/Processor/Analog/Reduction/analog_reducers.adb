with Reduction_Vectors;
use  Reduction_Vectors;

with Types.Float_Matrices;
use  Types.Float_Matrices;
use  Types.Float_Matrices.Float_Matrices_Pkg;

package body Analog_Reducers is

   ----------
   -- Init --
   ----------

   procedure Init
     (Reducer  : in out Analog_Reducer;
      Red_Vect : in     Analog_Vector_Idx;
      Block_Size : in     Positive)
   is
   begin
      Reducer := (Vector   => Get_Reduction_Vector(Red_Vect, Block_Size),
                  Block_Size => Block_Size,
                  Vector_Idx => Red_Vect);
   end Init;

   procedure Fill_Matrix (Mtx        : in out Float_Matrix;
                          Nrow, Ncol : in     Positive;
                          Payload    : in     Analog_Payload)
   is
      Idx : Positive;
      Padding : Positive := Nrow*Ncol - Payload'Length;
   begin
      Resize(Mtx, Nrow, Ncol);

      -- Fill the matrix with the values of the payload.
      -- Note that the last column of the matrix could contain
      -- some zeros added as a padding.  In order to avoid
      -- a lot of 'ifs', first fill the first Ncol-1 columns...
      Idx := 1;
      for Col in 1..Ncol-1 loop
         for Row in 1..Nrow loop
            Set(Mtx, Row, Col, Payload(Idx));
            Idx := Idx+1;
         end loop;
      end loop;

      -- ...then handle the last column
      -- First the un-padded part...
      for Row in 1..Nrow-Padding loop
         Set(Mtx, Row, Ncol, Payload(Idx));
         Idx := Idx+1;
      end loop;

      -- ...and finally the padded part
      for Row in Nrow-Padding+1..Nrow loop
         Set(Mtx, Row, Ncol, 0.0);
         Idx := Idx+1;
      end loop;
   end Fill_Matrix;


   -------------------
   -- Reduce_Packet --
   -------------------

   procedure Reduce_Packet
     (Reducer : in     Analog_Reducer;
      Packet  : in     Analog_Packet;
      Reduced :    out Reduced_Analog_Packet)
   is
      Nrow, Ncol : Positive;
      Mtx, Reduced_Payload : Float_Matrix;
      Padding : Natural;
   begin
      -- In order to reduce the payload
      Nrow := Size_Col(Reducer.Vector);

      Padding := Nrow - Packet.Payload'Length mod Nrow;
      if (Padding = Nrow) then
         Padding := 0;
      end if;

      Ncol := (Packet.Payload'Length+Padding) / Nrow;

      Fill_Matrix (Mtx, Nrow, Ncol, Packet.Payload.all);
      Reduced_Payload := Reducer.Vector * Mtx;

      Reduced := (Timestamp => Packet.Timestamp,
                  Portion   => Packet.Portion,
                  Payload   => new Analog_Payload(1..Ncol),
                  Padding   => Padding,
                  Vector    => Reducer.Vector_Idx,
                  Block_Size  => Reducer.Block_Size);

      Reduced.Payload.all := To_Coeff_Vector(Reduced_Payload);
   end Reduce_Packet;

end Analog_Reducers;

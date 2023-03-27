package body Profiles.Vandermonde.Processing.Reducers is
   function New_Reducer (Reduction_Basis : Binary_Vector_Idx;
                         Reduction_Fact  : Positive)
                        return Reducer_Type is
      Aux : GF256_Array(1..1) := (1 => Reduction_Basis);
      Result : Reducer_Type;
   begin
      return Reducer_Type'(Reduction_Vector =>
                             Wandermonde(Reduction_Factor, Reduction_Basis),
                           Reduction_Factor => Red_Fact;
                           Reduction_Basis  => Reduction_Basis);
   end Init;

   procedure Fill_Matrix (Mtx        : in out GF256_Matrix;
                          Nrow, Ncol : in     Positive;
                          Payload    : in     Binary_Payload)
   is
      Idx : Positive;
      Padding : Positive := Nrow*Ncol - Payload'Length;
      GF_Zero : GF256 := GF.Zero;
   begin
      Resize(Mtx, Nrow, Ncol);

      -- Fill the matrix with the values of the payload.
      -- Note that the last column of the matrix could contain
      -- some zeros added as a padding.  In order to avoid
      -- a lot of 'ifs', first fill the first Ncol-1 columns...
      Idx := 1;
      for Col in 1..Ncol-1 loop
         for Row in 1..Nrow loop
            GFM.Set(Mtx, Row, Col, Byte_To_GF256(Payload(Idx)));
            Idx := Idx+1;
         end loop;
      end loop;

      -- ...then handle the last column
      -- First the un-padded part...
      for Row in 1..Nrow-Padding loop
         GFM.Set(Mtx, Row, Ncol, Byte_To_GF256(Payload(Idx)));
         Idx := Idx+1;
      end loop;

      -- ...and finally the padded part
      for Row in Nrow-Padding+1..Nrow loop
         GFM.Set(Mtx, Row, Ncol, GF_Zero);
         Idx := Idx+1;
      end loop;
   end Fill_Matrix;

   function Reduce (Reducer : Reducer_Type;
                    Packet  : Galois_Packet)
                   return Reduced_Packet is
      Nrow, Ncol : Positive;
      Mtx, Red_Vector : GF256_Matrix;
      Padding : Natural;
      Reduced : Reduced_Packet;
   begin
      -- In order to reduce the payload
      Nrow := GFM.Size_Col(Reducer.Vector);

      Padding := Nrow - Packet.Payload'Length mod Nrow;
      if (Padding = Nrow) then
         Padding := 0;
      end if;

      Ncol := (Packet.Payload'Length+Padding) / Nrow;

      Fill_Matrix (Mtx, Nrow, Ncol, Packet.Payload.all);
      Red_Vector := Reducer.Vector * Mtx;

      Reduced := (Timestamp => Packet.Timestamp,
                  portion   => Packet.Portion,
                  Payload   => new Binary_Payload(1..GFM.Size_Col(Red_Vector)),
                  Padding   => Padding,
                  Vector    => Reducer.Vector_Idx,
                  Block_size => Size(Reducer.Vector));

      for I in Reduced.Payload'Range loop
         Reduced.Payload(I) := Byte(GF.To_Int(GFM.Get(Red_Vector, 1, I)));
      end loop;

      return Reduced;
   end Reduce;

   function Details (Reducer : Reducer_Type)
                    return Vandermonde.Details is
   begin
      return Vandermonde.Details'(Galois_Field     => GF_16,
                             Reduction_Factor => Reducer.Reduction_Factor,
                             Reduction_Vector => Reducer.Reduction_Basis,
                             Padded           => False);
   end Details;

end Profiles.Vandermonde.Processing.Reducers;

package Profiles.Vandermonde.Processing.Reducers is
   type Reducer_Type is tagged private;

   function New_Reducer (Reduction_Basis : Galois;
                         Reduction_Fact  : Positive)
                        return Reducer_Type;

   function Reduce (Reducer : Reducer_Type;
                    Packet  : Raw_Packet)
                   return Data_Packet;

   function Details (Reducer : Reducer_Type)
                    return Vandermonde.Details;
private
   type Reducer_Type is tagged
      record
         Reduction_Vector : Galois_Matrix;
         Reduction_Factor : Positive;
         Reduction_Basis  : Galois;
      end record;
end Profiles.Vandermonde.Processing.Reducers;

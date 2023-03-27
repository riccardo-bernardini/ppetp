with Packets.Components.Analog;
use  Packets.Components.Analog;

with Packets.Reduced.Analog;
use  Packets.Reduced.Analog;


with Types.Float_Matrices;
use  Types.Float_Matrices;
use  Types.Float_Matrices.Float_Matrices_Pkg;

package Analog_Reducers is
   type Analog_Reducer is private;

   procedure Init (Reducer    : in out Analog_Reducer;
                   Red_Vect   : in     Analog_Vector_Idx;
                   Block_Size : in     Positive);

   procedure Reduce (Reducer : in     Analog_Reducer;
                     Packet  : in     Analog_Packet;
                     Reduced :    out Reduced_Analog_Packet);
private

   type Analog_Reducer is record
      Vector     : Float_Matrix;
      Block_Size : Positive;
      Vector_Idx : Analog_Vector_Idx;
   end record;
end Analog_Reducers;

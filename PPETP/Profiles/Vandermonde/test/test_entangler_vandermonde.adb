-- This is a complete test for package Profiles.Entanglers.Vandermonde.
-- Running this procedure a series of 36 tests will rised, for improveing
-- package Profiles.Entanglers.Vandermonde. The chosen values for "reduction
-- factor" and "length of input data" insure the test of all possible
-- combinations of internal matrices in the entanlger package. This is true
-- also for higer values of reduction factor and length of input data.

with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Entanglers.Vandermonde;   use Profiles.Entanglers.Vandermonde;
with Profiles.Parameters;	        use Profiles.Parameters;
with Profiles. Entangled;	        use Profiles.Entangled;
with Profiles;				use Profiles;
with PPETP;                             use PPETP;

with Byte_Arrays;                       use Byte_Arrays;
with Galois_Matrices;   		use Galois_Matrices;
with Galois_Field;			use Galois_Field;
with Galois_Arrays; 			use Galois_Arrays;
with Packets.Binary.Application; 	use Packets.Binary.Application;
with Test_Report;       		use Test_Report;


with Interfaces;              		use Interfaces;
with Ada.Streams;  			use Ada.Streams;
with Ada.Text_IO; 			use Ada.Text_IO;
with Ada.Unchecked_Conversion;



procedure Test_Entangler_Vandermonde is


   -- "CREATE" FUNCTIONS --

   function Create_Parameter (Reduction_Factor : Natural;
                              Galois_Element : Integer;
                              GF_Size          : Natural := 32)
                              return Parameters_Class_Pt is
      P : Parameters_Class_Pt := new Vandermonde_Par;
   begin
      Vandermonde_Par(P.all).Reduction_Factor := Reduction_Factor;
      Vandermonde_Par(P.all).Galois_Element :=  To_Galois(Galois_Element);
      Vandermonde_Par(P.all).GF_Size := GF_Size;
      return P;
   end Create_Parameter;



   function Create_Application_Packet (Moment : Timestamp_Type ; Input : Byte_Array)
                                       return Application_Packet is
      Packet : Application_Packet;
   begin
      Packet := New_Packet (Timestamp => Moment,
                           Data  => Input);
      return Packet;
      -- Ecco la parte di codice che non funziona (RAISED PROGRAM ERROR)
      -- sostituita con la sovrastante (AS 2009/07/02)
      --        return New_Packet (Timestamp => Moment,
      --                           Data  => Input);
   end Create_Application_Packet;

   -- PARAMETERS --
   Reporter : Reporter_Type;

   Fattore1 : Integer := 2 ;
   Fattore2 : Integer := 8 ;
   Fattore3 : Integer := 16 ;
   Fattore4 : Integer := 18 ;
   Fattore5 : Integer := 33 ;
   Fattore6 : Integer := 64 ;


   -- Data1: Byte_Array_Pt := new Byte_Array(1 .. 2);
   Data1: Byte_Array_Pt := new Byte_Array(1 .. 8);
   Data2: Byte_Array_Pt := new Byte_Array(1 .. 8);
   Data3: Byte_Array_Pt := new Byte_Array(1 .. 7);
   Data4: Byte_Array_Pt := new Byte_Array(1 .. 32);
   Data5: Byte_Array_Pt := new Byte_Array(1 .. 64);
   Data6: Byte_Array_Pt := new Byte_Array(1 .. 232);


   type Entangler_Case is
      record
         Fact : Integer;
         Data : Byte_Array_Pt;
      end record;

   type Entangler_Case_Array is array (Positive  range <>) of Entangler_Case;

   Cases_Param : Entangler_Case_Array(1 .. 36)  ;




   ------- FUNCTION ENTANGLER_TEST -----------
   function Entangler_Test (X : Entangler_Case) return Boolean is
      Time : Timestamp_Type := 10;
      Pac  : Application_Packet;
      R    : Matrix;
      Rinv : Matrix;
      M    : Matrix;
      U    : Matrix;
      Col  : Integer;
      End_Value : Integer;

      Ent_Obj   : Entangler_Pt;
      Init_Pt   : Parameters_Class_Pt;
      Res       : Entangled_Data;
      Medium    : Byte_Array_Pt;
      Result    : Byte_Array_Pt;
      Fattore   : Integer := X.Fact;
      Elementi  : Array_1D(1 .. Fattore);
      Data      : Byte_Array_Pt;


   -- Conversion Functions --
   function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);
   ---
   function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);
   ---
   function Stream_Array_To_Byte_Array (A :  Ada.Streams.Stream_Element_Array)
                                        return Byte_Array is
      H : Byte_Array (1 .. A'Length);
      function Convert is
        new Ada.Unchecked_Conversion (Source => Ada.Streams.Stream_Element,
                                      Target => Unsigned_8);
   begin
      for K in 1 .. A'Length
        loop
         H(To_Stream_Offset(K)) := A(To_Stream_Offset(K));
      end loop;
      return H;
   end Stream_Array_To_Byte_Array;
   ---
   function Galois_Matrix_To_Byte_Array( M : Matrix ) return Byte_Array
   is
      Col : Integer := Galois_Matrices.Size_Col(M);
      Row : Integer := Galois_Matrices.Size_Row(M);
      Temp : Galois_Array(1 .. Row);
      Temp3 : Stream_Element_Array(To_Stream_Offset(1) .. To_Stream_Offset(4 * Col * Row));
   begin
      for I in 1 .. Col
      loop
         for J in 1 .. Row
         loop
            Temp(J) := Galois_Matrices.Get(M, J, I);
         end loop;
         declare
            Temp2 : Stream_Element_Array := Galois_Array_To_Stream(Temp);
            Lun   : Integer := Temp2'Length;
         begin
            Temp3(To_Stream_Offset(1 + (i - 1) * Lun) .. To_Stream_Offset(Lun * i)) := Temp2;
         end;
      end loop;
      return Stream_Array_To_Byte_Array(Temp3);
   end Galois_Matrix_To_Byte_Array;



   begin
     -- Creation of a vector of elements of galois --
   for K in 1 .. Fattore
     loop
      Elementi(K) := To_Galois(Integer( K ));
      end loop;
      ---------------------------------------------
      Data := X.Data;

      Pac := Create_Application_Packet (Time  , Data.all );

   for T in 1 .. Fattore
   loop
      -- Entangler Object --
      Ent_Obj := New_Entangler;
      Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
                                  Galois_Element => Integer(To_int(Elementi(T))));
      Set_Default(Ent_Obj.all , Init_Pt);
      Entangle(Ent_Obj.all, Pac , Res);
      -- ***************** --

      Col := Size_Col(Vandermonde_Ent(Res.Payload.all).Galois_Data);
      if T = 1 then
         U := Galois_Matrices.Create(Fattore , Col);
      end if;
         for I in 1 .. Col
         loop
            Galois_Matrices.Set(U, T, I, Get(Vandermonde_Ent(Res.Payload.all).Galois_Data , 1, I));
         end loop;
      end loop;

   -- Reconstruction --

   Wandermonde(R, Fattore, Elementi);

   Rinv := Inv(R);

   --        U := R * M;
   -- Rinv * U := Rinv * R * M;
   -- Rinv * U := M

   M := Rinv * U;


   Medium := new Byte_Array'(Galois_Matrix_To_Byte_Array(M));

   if Vandermonde_Ent(Res.Payload.all).Padding then
      if Medium(Medium'Length) / 128 > 0 then
         End_Value := Integer(Medium'Length) - 128 * Integer(Medium(Medium'Last - 1))
                                             - (Integer(Medium(Medium'Last)) - 128);
      else
         End_Value := Integer(Medium'Length) - Integer(Medium(Medium'Last));
      end if;
   else
      End_Value := Integer(Medium'Length);
   end if;




   Result := new Byte_Array(1 .. To_Stream_Offset(End_Value));
   Result.all := Medium (1 .. To_Stream_Offset(End_Value));


   if Result.all = Data.all then
      return True;
      else
         return False;
      end if;
   end Entangler_Test;




   procedure Test_Function_Entangler is
     new Do_Suite(Test_Case => Entangler_Case,
                  Test_Case_Array => Entangler_Case_Array,
                  Check => Entangler_Test);




begin


   -- Data1.all := (1, 0);
   Data1.all := (1, 2, 3, 4, 5, 6, 7, 8);

   Data2.all := (1, 2, 3, 4, 5, 6, 7, 8);

   Data3.all := (1, 2, 3, 4, 5, 6, 7);

   Data4.all := (1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 9, 10, 11, 12, 13, 14, 15, 16);

   Data5.all := (1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8);

   Data6.all  := (1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                 1, 2, 3, 4, 5, 6, 7, 8,
                                   1, 2, 3, 4, 5, 6, 7, 8);

      Cases_Param := ((Fact => Fattore1, Data => Data1),
                      (Fact => Fattore1, Data => Data2),
                      (Fact => Fattore1, Data => Data3),
                      (Fact => Fattore1, Data => Data4),
                      (Fact => Fattore1, Data => Data5),
                      (Fact => Fattore1, Data => Data6),
                      (Fact => Fattore2, Data => Data1),
                      (Fact => Fattore2, Data => Data2),
                      (Fact => Fattore2, Data => Data3),
                      (Fact => Fattore2, Data => Data4),
                      (Fact => Fattore2, Data => Data5),
                      (Fact => Fattore2, Data => Data6),
                      (Fact => Fattore3, Data => Data1),
                      (Fact => Fattore3, Data => Data2),
                      (Fact => Fattore3, Data => Data3),
                      (Fact => Fattore3, Data => Data4),
                      (Fact => Fattore3, Data => Data5),
                      (Fact => Fattore3, Data => Data6),
                      (Fact => Fattore4, Data => Data1),
                      (Fact => Fattore4, Data => Data2),
                      (Fact => Fattore4, Data => Data3),
                      (Fact => Fattore4, Data => Data4),
                      (Fact => Fattore4, Data => Data5),
                      (Fact => Fattore4, Data => Data6),
                      (Fact => Fattore5, Data => Data1),
                      (Fact => Fattore5, Data => Data2),
                      (Fact => Fattore5, Data => Data3),
                      (Fact => Fattore5, Data => Data4),
                      (Fact => Fattore5, Data => Data5),
                      (Fact => Fattore5, Data => Data6),
                      (Fact => Fattore6, Data => Data1),
                      (Fact => Fattore6, Data => Data2),
                      (Fact => Fattore6, Data => Data3),
                      (Fact => Fattore6, Data => Data4),
                      (Fact => Fattore6, Data => Data5),
                      (Fact => Fattore6, Data => Data6)
                                             );


   Test_Function_Entangler(Reporter , Cases_Param);


   Final(Reporter);

end Test_Entangler_Vandermonde;


-- This is a pratical test for package Profiles.Entanlgers.Vandermonde.
-- It gives a visual output of the test results. By setting the values of
-- End_Fattore and End_Lunghezza you can test the package for ALL values
-- of reduction factor between 2 and End_Fattore combined with generic data input
-- with length between 2 and End_Lunghhezza. For every line on the standard
-- output you can see the values of reduction factor and input data length and
-- the relative result of the test (true meand the test is passed).

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
with Ada.Integer_Text_IO; 		use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;



procedure Scan_Test_Entangler_Vandermonde is

   End_Fattore : Integer := 64;
   End_Lunghezza : Ada.Streams.Stream_Element_Offset := 60;


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



--     function Create_Application_Packet (Time : Timestamp_Type ; Input : Byte_Array)
--                                         return Application_Packet is
--     begin
--        return New_Packet (Timestamp => Time,
--                          Data  => Input);
--     end Create_Application_Packet;

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



begin

   Put_Line("   REDUCTION FACTOR  |   INPUT DATA LENGTH   |   RESULT OF TEST ");


   for N_Fattore in 2 .. End_Fattore
   loop
      for N_Lunghezza in 2 .. End_Lunghezza
        loop

         declare

   -- Parametri --
   Fattore : Integer := N_Fattore;
   Lunghezza : Ada.Streams.Stream_Element_Offset := N_Lunghezza;
   Elementi : Array_1D(1 .. Fattore);
   Data: Byte_Array(1 .. Lunghezza);

   Time : Timestamp_Type := 10;
   Pac : Application_Packet;
   R : Matrix;
   Rinv : Matrix;
   M : Matrix;
   U : Matrix;
   Col : Integer;
   End_Value : Integer;

   Ent_Obj : Entangler_Pt;
   Init_Pt : Parameters_Class_Pt;
   Res : Entangled_Data;

   Medium : Byte_Array_Pt;
   Result : Byte_Array_Pt;


   function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);


   function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);


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

   -- Creazione di Elementi --
   for K in 1 .. Fattore
     loop
      Elementi(K) := To_Galois(Integer( K ));
   end loop;

   --Creazione di Dati
   for H in 1 .. Lunghezza
     loop
      Data(To_Stream_Offset(Integer(H))) := To_Stream(Interfaces.Unsigned_8(H) mod 128);
   end loop;


   Pac := Create_Application_Packet (Time  , Data );



   for T in 1 .. Fattore
   loop
      -- Oggetto entangler --
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



   -- RICOSTRUZIONE --

   Wandermonde(R, Fattore, Elementi);

   Rinv := Inv(R);

   --        U := R * M;
   -- Rinv * U := Rinv * R * M;
   -- Rinv * U := M

   M := Rinv * U;

   Medium := new Byte_Array'(Galois_Matrix_To_Byte_Array(M));

--     if Vandermonde_Ent(Res.Payload.all).Padding then
--        if Medium(Medium'Length) = 0 then
--           End_Value := Integer(Medium'Length) - 128 * Integer(Medium(Medium'Last -1))
--                                               - Integer(Medium(Medium'Last - 2));
--        else
--           End_Value := Integer(Medium'Length) - Integer(Medium(Medium'Last));
--        end if;
--     else
--        End_Value := Integer(Medium'Length);
--     end if;

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


            New_Line; Put(Fattore);
            Put("            ");
            Put(Integer(Lunghezza));
            Put("                  ");

   if Result.all = Data then
Put("True");
   else
      New_Line; Put("False");
            end if;
         end;

         end loop;
      end loop;

end Scan_Test_Entangler_Vandermonde;


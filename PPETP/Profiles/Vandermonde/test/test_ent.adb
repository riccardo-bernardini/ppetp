with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Builders.Vandermonde;     use Profiles.Builders.Vandermonde;
with Profiles.Parsers.Vandermonde;      use Profiles.Parsers.Vandermonde;
with Profiles.Entanglers.Vandermonde;   use Profiles.Entanglers.Vandermonde;
with Profiles.Parameters;	        use Profiles.Parameters;
with Profiles.Parsers;		        use Profiles.Parsers;
with Profiles. Entangled;	        use Profiles.Entangled;
with Profiles;				use Profiles;
with PPETP;         use PPETP;


with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Parsing_Buffers;   use Parsing_Buffers;
with Packets.Binary.Application; use Packets.Binary.Application;
with Generic_Matrix;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;  use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

with Create;  use Create;
with View;    use View;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;



procedure Test_Ent is

   -- Parametri --
   Fattore : Integer := 2;
   type Gal_Vet is array (1 .. Fattore) of Integer;
   -- Data: Byte_Array(1 .. 2) := (1, 0); --
   Data: Byte_Array(1 .. 8) := (1, 2, 3, 4, 5, 6, 7, 8);
   -- Data: Byte_Array(1 .. 16) := (1, 2, 3, 4, 5, 6, 7, 8,
                                 --9, 10, 11, 12, 13, 14, 15, 16);
   Elementi : Array_1D(1 .. 2) := (To_Galois(Integer(5)) ,To_Galois(Integer(6)));
   -- ********* --


   Time : Timestamp_Type := 10;
   Pac : Application_Packet;
   R : Matrix;
   Rinv : Matrix;
   M : Matrix;
   U : Matrix;
   Col : Integer;



   Ent_Obj : Entangler_Pt;
   Init_Pt : Parameters_Class_Pt;
   Res : Entangled_Data;

   Ent_Obj2 : Entangler_Pt;
   Init_Pt2 : Parameters_Class_Pt;
   Res2 : Entangled_Data;
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
     --type Stream_Element_Array_Pt is access Stream_Element_Array;
      Temp3 : Stream_Element_Array(To_Stream_Offset(1) .. To_Stream_Offset(4 * Col * Row));
      --Count : Integer := 1;
   begin
     -- New_Line; Put(Col); Put(Row);
      for I in 1 .. Col
      loop
         for J in 1 .. Row
         loop
            Temp(J) := Galois_Matrices.Get(M, J, I);
            --New_Line; Put(Integer(To_Int(Temp(J))));
         end loop;
         declare
            Temp2 : Stream_Element_Array := Galois_Array_To_Stream(Temp);
            Lun   : Integer := Temp2'Length;
         begin
          --  New_Line;Put(Lun); Put(Temp'Length);
            Temp3(To_Stream_Offset(1 + (i - 1) * Lun) .. To_Stream_Offset(Lun * i)) := Temp2;
            --Count := Count + 1;
         end;
      end loop;
      return Stream_Array_To_Byte_Array(Temp3);
   end Galois_Matrix_To_Byte_Array;





begin
   View_Byte_Array(Data); New_Line;

   Pac := Create_Application_Packet (Time  , Data );

   -- Oggetto entangler --
   Ent_Obj := New_Entangler;
   Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
                               Galois_Element => Integer(To_int(Elementi(1))));
   Set_Default(Ent_Obj.all , Init_Pt);
   Entangle(Ent_Obj.all, Pac , Res);
   -- ***************** --


   -- Oggetto entangler 2--
   Ent_Obj2 := New_Entangler;
   Init_Pt2 := Create_Parameter(Reduction_Factor =>Fattore,
                               Galois_Element => Integer(To_int(Elementi(2))));
   Set_Default(Ent_Obj2.all , Init_Pt2);
   Entangle(Ent_Obj2.all, Pac , Res2);
   -- ***************** --



   -- RICOSTRUZIONE --

   Wandermonde(R, Fattore, Elementi);
--        New_Line; Put("Matrice di Vandermonde R  ************************");
--     View_Matrix(R);
   Rinv := Inv(R);
--           New_Line; Put("Matrice di Vandermonde R invertita  ************************");
--     View_Matrix(Rinv);
   Col := Size_Col(Vandermonde_Ent(Res.Payload.all).Galois_Data);



   U := Galois_Matrices.Create(Fattore , Col);

   for I in 1 .. Col
     loop
      Galois_Matrices.Set(U, 1, I, Get(Vandermonde_Ent(Res.Payload.all).Galois_Data , 1, I));
   end loop;

   for I in 1 .. Col
     loop
      Galois_Matrices.Set(U, 2, I, Get(Vandermonde_Ent(Res2.Payload.all).Galois_Data , 1, I));
   end loop;

   New_Line; put("Matrice dei vettori ridotti  ************************");

   View_Matrix(U);

   --        U := R * M;
   -- Rinv * U := Rinv * R * M;
   -- Rinv * U := M

   M := Rinv * U;

--     New_Line;put("Risultato  ************************");
--     View_Matrix(M);

      -- View_Byte_Array(Galois_Matrix_To_Byte_Array(M));

   Medium := new Byte_Array'(Galois_Matrix_To_Byte_Array(M));

   Result := new Byte_Array(1 .. Medium'Length - To_Stream_Offset(Integer(Medium(Medium'Last))));

   Result.all := Medium (1 .. Medium'Length - To_Stream_Offset(Integer(Medium(Medium'Last))));

  View_Byte_Array(Result);





end Test_Ent;


--  C:\PPETP\Profiles\Vandermonde\test\obj\test_ent
--
--  Lunghezza :           8
--  Byte_Array : (visualizzazione a gruppi di 4 byte)
--         1       2       3       4
--         5       6       7       8
--
--
--  Matrice dei vettori ridotti  ************************
--
--  Righe / Colonne :           2          8
--
--  Matrix :
--   739777552      0      0      0      0      0      0      3623878656
--   873535007      0      0      0      0      0      0      2415919104
--
--  Lunghezza :           8
--  Byte_Array : (visualizzazione a gruppi di 4 byte)
--         1       2       3       4
--         5       6       7       8
--  [2009-02-16 23:17:05] process terminated successfully (elapsed time: 00.39s)




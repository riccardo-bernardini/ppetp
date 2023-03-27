-- Questo file contiene delle funzioni utili per la visualizzazione
-- dei dati durante la fase di test. in pratica è la gestione dello standard
-- output per i vary Byte_Array, Matrix... ecc...
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;

with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Packets.Binary.Application; 	use Packets.Binary.Application;
with Packets.Binary;   use Packets.Binary;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;


with Profiles.Disentanglers.Vandermonde; use Profiles.Disentanglers.Vandermonde;
with Disentangler_Input_Queue; use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;   use Disentangler_Internal_Queue;
with Utility_Queue;   use Utility_Queue;



package body View is

   procedure View_Byte_Array (Input : Byte_Array_Pt)
   is
   begin
            New_Line; Put("Lunghezza : ") ; Put(Input'Length);
      New_Line; Put("Byte_Array : (visualizzazione a gruppi di 4 byte)"); New_Line;
      for I in Input'First .. Input'Last
        loop
         --Put(Integer(I));  Put("     ");
         Put(Integer(Input(I)) , 8 , 10);
           if (Integer(I) - Integer(Input'First) + 1) mod 4 = 0 then
              New_Line;
             end if;
      end loop;

   end View_Byte_Array;



   procedure View_Byte_Array (Input : Byte_Array)
   is
   begin
            New_Line; Put("Lunghezza : ") ; Put(Input'Length);
      New_Line; Put("Byte_Array : (visualizzazione a gruppi di 4 byte)"); New_Line;
      for I in Input'First .. Input'Last
        loop
         Put(Integer(Input(I)) , 8 , 10);
         if Integer(I) mod 4 = 0 then
            New_Line;
            end if;
      end loop;

   end View_Byte_Array;


     procedure View_Galois_Array (Input : Galois_Array)
   is
   begin
      New_Line; Put("Lunghezza : ") ; Put(Input'Length);
      New_Line; Put("Galois_Array :"); New_Line;
      for I in Input'First .. Input'Last
      loop
         Put(Integer(To_Int(Input(I))) , 15 , 10);
--           if Integer(I) mod 4 = 0 then
             New_Line;
--              end if;
      end loop;

   end View_Galois_Array;



   procedure View_Byte_Array_in_bit (Input : Byte_Array_Pt)
   is
   begin
      New_Line;
      for I in Input'First .. Input'Last
        loop
         Put(Integer(I));  Put("     "); Put(Integer(Input(I)) , 8 , 2); New_Line;
      end loop;

   end View_Byte_Array_In_Bit;

   Procedure View_Boolean (Input : Boolean) is
   begin
     -- New_Line;
      if Input then
         Put("True");
      else
         Put("False");
      end if;
      New_Line;
   end View_Boolean;

   Procedure View_Profile_Flags (Input : Profile_Flags) is
      A : Integer := Integer(Input);
   begin
      New_Line;
      Put(A / 4, 8 , 10); A := A mod 4;
      Put(A / 2, 8 , 10); A := A mod 2;
      Put(A , 8 , 10);
      New_Line;
   end View_Profile_Flags;



   Procedure View_Parameters (Input : Parameters_Class_Pt)
   is
   begin
      New_Line;
      Put("Reduction_Factor : ");
      Put(Vandermonde_Par(Input.all).Reduction_Factor , 8 , 10);
      New_Line;
            Put("Galois_Element : ");
      Put(Integer(To_Int(Vandermonde_Par(Input.all).Galois_Element)) , 8 , 10);
      New_Line;
            Put("GF_Size : ");
      Put(Vandermonde_Par(Input.all).GF_Size , 8 , 10);
      New_Line;
   end View_Parameters ;

   procedure View_Entangled (Input : Entangled_Payload_Pt) is
      Nrow : Natural := Size_Row(Vandermonde_Ent(Input.all).Galois_Data);
      Ncol : Natural := Size_Col(Vandermonde_Ent(Input.all).Galois_Data);

   begin
      New_Line; Put("-- ENTANGLED --");
      New_Line; Put("Galois_Data : "); New_Line;
      for I in 1 .. Nrow
      loop
         for J in 1 .. Ncol
         loop
            Put(Image(Get(Vandermonde_Ent(Input.all).Galois_Data, I, J)));
         end loop;
         New_Line;
      end loop;
      New_Line; Put("Padding : ");
      View_Boolean(Vandermonde_Ent(Input.all).Padding);
      New_Line; Put("-- PARAMETERS --"); View_Parameters(Vandermonde_Ent(Input.all).Param);

      New_Line;
   end View_Entangled;

   Procedure View_Matrix_Cell (Input : Matrix) is
   begin
      New_Line; Put("Matrix : "); New_Line;
      Put(Integer(To_Int(Get(Input, 1, 1)))); New_Line;
   end View_Matrix_Cell;



   Procedure View_Matrix (Input : Matrix) is
   begin
      New_Line; New_Line; Put("Righe / Colonne : ");
      Put (Size_Row(Input));
      Put(Size_Col(Input));New_Line;New_Line;


      Put_Line("Matrix : ");
      for I in 1 .. Size_Row(Input)
      loop
         for J in 1 .. Size_Col(Input)
         loop
            --         Ada.Long_Integer_Text_IO.Put(Long_Integer(To_Int(Get(Input, I, J))));
            Put(Unsigned_64'Image(To_Int(Get(Input, I, J)))); Put("     ");
         end loop;
         New_Line;
      end loop;

      end View_Matrix ;

   procedure View_Input_Queue(Input : Disentangler_Input_Queue.Map) is
      Temp : Entangled_Record;
   begin
      if Integer(Length(Input)) = 0 then
         new_Line; put_line ("La coda di INPUT è vuota");
      else
         for I in 1 .. Integer(Length(Input))
         loop
            put("ELEMENTO N. "); put(I); New_Line;
            Temp := Element(Input, Integer_Timestamp(I));
            put("Occupation: "); put(Temp.Occupation);
            for J in 1 .. Temp.Occupation
            loop
               view_Entangled(Temp.Vectors(J));new_line;
               Put_line("-----");
            end loop;
         end loop;


      end if;
   end View_Input_Queue;


      procedure View_Queue(Input : Disentangler_Internal_Queue.Map) is
      Temp : Queue_Element;
   begin
      if Integer(Length(Input)) = 0 then
         new_Line; put_line ("La coda INTERNA è vuota");
      else
         for I in 1 .. Integer(Length(Input))
         loop
            new_Line; put("*********ELEMENTO N. "); put(I); New_Line;

            Temp := Element(Input, Integer_Timestamp(I));
            put("*********Timestamp:"); Put(Integer(Temp.Timestamp)); new_Line;
            -- View_Byte_Array(Buffer(Binary_Packet(Temp.Binary_Payload)));

         end loop;


      end if;
   end View_Queue;







end View;

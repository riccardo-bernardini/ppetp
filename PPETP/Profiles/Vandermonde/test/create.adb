-- Questo file contiene delle funzioni utili per la creazione
-- dei dati durante la fase di test.
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;

with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Parsing_Buffers;   use Parsing_Buffers;
with Packets.Binary.Application; use Packets.Binary.Application;
with PPETP;      use PPETP;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;



package body create is

   ----------------------------
      --- CONVERSION FUNCTIONS ---

      function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);

      function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);


   function Create_Parameter (Reduction_Factor : Natural;
                              Galois_Element : Integer;
                              GF_Size          : Natural := 32)
                              return Parameters_Class_Pt is
      P : Parameters_Class_Pt := new Vandermonde_Par;
   begin
      Vandermonde_Par(P.all).Reduction_Factor := Reduction_Factor;
      Vandermonde_Par(P.all).Galois_Element :=  To_Galois(Galois_Element);
      Vandermonde_Par(P.all).GF_Size := GF_Size;
      -- Vandermonde_Par(P.all).My_Profile := Vandermonde_Profile;

      return P;
   end Create_Parameter;




   function Create_Matrix_Cell (Value : Integer) return Matrix is
     A : Galois := To_Galois(Value);
     Result : Matrix := Galois_Matrices.Create(1 , 1);
   begin
      Galois_Matrices.Set(Result, 1 , 1 , A);
      return Result;
   end Create_Matrix_Cell;



   function Create_Entangled (Galois_Data : Matrix;
                              Param       : Parameters_Class_Pt;
                              Padding      : Boolean := False)
                              return Entangled_Payload_Pt is
      E : Entangled_Payload_Pt := new Vandermonde_Ent;
   begin

      Vandermonde_Ent(E.all).Galois_Data := Galois_Data;
      Vandermonde_Ent(E.all).Param := Param;
      Vandermonde_Ent(E.all).Padding := Padding;
      -- Vandermonde_Ent(E.all).My_Profile := Vandermonde_Profile;

      return E;

   end Create_Entangled;

   function Create_Byte_Array( Nrow: Positive; Value : Integer) return byte_array_Pt
   is
      Result : Byte_Array_Pt := new Byte_Array (To_Stream_Offset(1) .. To_Stream_Offset(Nrow)) ;
   begin
      for I in 1 .. Nrow
      loop
         Result(To_Stream_Offset(I)) := To_Stream(Unsigned_8(Value));
      end loop;

      return Result;
   end Create_Byte_Array;



   function Create_Application_Packet (Time : Timestamp_Type ; Input : Byte_Array)
                                       return Application_Packet is
   begin
      return New_Packet (Timestamp => Time,
                        Data  => Input);

     end Create_Application_Packet;



   function Create_Entangled_Data (Galois_Data1 : Matrix;
                                      Param1       : Parameters_Class_Pt;
                                      Padding1      : Boolean := False;
                                      Time : Timestamp_Type)
                                      return Entangled_Data is
      A : Entangled_Data;
   begin
      A.Timestamp := Time;
      A.Payload :=  Create_Entangled (Galois_Data1, Param1, Padding1);
      return A;
   end Create_Entangled_Data;






end create;

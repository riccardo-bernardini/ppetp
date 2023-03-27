-- Questo file contiene delle funzioni utili per la creazione
-- dei dati durante la fase di test.
with Profiles.Entangled;    use Profiles.Entangled;
with Profiles.Parameters;   use Profiles.Parameters;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles;   use Profiles;
with PPETP;      use PPETP;

with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Parsing_Buffers;   use Parsing_Buffers;
with Packets.Binary.Application; use Packets.Binary.Application;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;



package create is

   function Create_Parameter (Reduction_Factor : Natural;
                              Galois_Element : Integer;
                              GF_Size          : Natural := 32)
                             return Parameters_Class_Pt;

   function Create_Matrix_Cell (Value : Integer) return Matrix ;

   function Create_Entangled (Galois_Data : Matrix;
                              Param       : Parameters_Class_Pt;
                              Padding      : Boolean := False)
                              return Entangled_Payload_Pt;

      function Create_Entangled_Data (Galois_Data1 : Matrix;
                                      Param1       : Parameters_Class_Pt;
                                      Padding1      : Boolean := False;
                                      Time : Timestamp_Type)
                                 return Entangled_Data;

   function Create_Byte_Array( Nrow: Positive; Value : Integer) return Byte_Array_Pt;

   function Create_Application_Packet (Time : Timestamp_Type ; Input : Byte_Array)
                                       return Application_Packet;




end create;

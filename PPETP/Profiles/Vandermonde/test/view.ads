-- Questo file contiene delle funzioni utili per la visualizzazione
-- dei dati durante la fase di test. in pratica è la gestione dello standard
-- output per i vary Byte_Array, Matrix... ecc...
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles;			        use Profiles;
with Profiles.Parameters;     	  	use Profiles.Parameters;
with Profiles.Entangled;     	  	use Profiles.Entangled;


with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Disentangler_Input_Queue; use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;   use Disentangler_Internal_Queue;
with Utility_Queue;   use Utility_Queue;



package View is

   procedure View_Byte_Array (Input : Byte_Array_Pt);

   procedure View_Byte_Array (Input : Byte_Array);

   procedure View_Galois_Array (Input : Galois_Array);

   procedure View_Byte_Array_In_Bit (Input : Byte_Array_Pt);

   Procedure View_Boolean (Input : Boolean);

   Procedure View_Profile_Flags (Input : Profile_Flags);

   Procedure View_Parameters (Input : Parameters_Class_Pt);

   procedure View_Entangled (Input : Entangled_Payload_Pt);

   Procedure View_Matrix_Cell (Input : Matrix);

   Procedure View_Matrix (Input : Matrix);

   procedure View_Input_Queue(Input : Disentangler_Input_Queue.Map);

   procedure View_Queue(Input : Disentangler_Internal_Queue.Map);










end View;

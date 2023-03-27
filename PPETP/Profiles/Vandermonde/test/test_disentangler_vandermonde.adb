-- This is a complete test for package Profiles.Disentangler.Vandermonde.

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

with Profiles.Disentanglers.Vandermonde;  use Profiles.Disentanglers.Vandermonde;
with Profiles.Disentanglers; 		  use Profiles.Disentanglers;
with Disentangler_Input_Queue; 		  use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;  	  use Disentangler_Internal_Queue;
with Utility_Queue;   			  use Utility_Queue;
with Packets.Binary.Application; 	  use Packets.Binary.Application;
with Packets.Binary;  			  use Packets.Binary;


with Interfaces;              		use Interfaces;
with Ada.Streams;  			use Ada.Streams;
with Ada.Text_IO; 			use Ada.Text_IO;
with Ada.Unchecked_Conversion;



procedure Test_Disentangler_Vandermonde is


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
   -----------------------------------------------------------------------------
   function Create_Application_Packet (Moment : Timestamp_Type ; Input : Byte_Array)
                                       return Application_Packet is
      Packet : Application_Packet;
   begin
      Packet := New_Packet (Timestamp => Moment,
                           Data  => Input);
      return Packet;
   end Create_Application_Packet;



   -- CONVERSION FUNCTIONS --
   function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);
   -----------------------------------------------------------------------------
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
   -----------------------------------------------------------------------------
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
   -----------------------------------------------------------------------------


   -- PARAMETERS --
   Reporter : Reporter_Type;

   Fattore1 : Integer := 2 ;
   Fattore2 : Integer := 8 ;
   Fattore3 : Integer := 16 ;
   Fattore4 : Integer := 18 ;
   Fattore5 : Integer := 33 ;
   Fattore6 : Integer := 64 ;


   Data1: Byte_Array_Pt := new Byte_Array(1 .. 2);
   Data2: Byte_Array_Pt := new Byte_Array(1 .. 8);
   Data3: Byte_Array_Pt := new Byte_Array(1 .. 7);
   Data4: Byte_Array_Pt := new Byte_Array(1 .. 32);
   Data5: Byte_Array_Pt := new Byte_Array(1 .. 64);
   Data6: Byte_Array_Pt := new Byte_Array(1 .. 232);


   type Entangler_Case is
      record
         Fact : Integer;
         Data : Byte_Array_Pt;
         Times : Timestamp_Type;
      end record;

   type Entangler_Case_Array is array (Positive  range <>) of Entangler_Case;

   Cases_Param : Entangler_Case_Array(1 .. 36)  ;



   ------- FUNCTION PROCESS_TEST -----------
   function Process_Test (X : Entangler_Case) return Boolean is
      Time : Timestamp_Type := X.Times;
      Pac  : Application_Packet;
      Pac2 : Application_Packet;

      Ent_Obj   : Entangler_Pt;
      Init_Pt   : Parameters_Class_Pt;
      Res       : Entangled_Data;

      Fattore   : Integer := X.Fact;
      Elementi  : Array_1D(1 .. Fattore);
      Data      : Byte_Array_Pt;

      Dis_Object : Disentangler_Pt := New_Disentangler;
      Control    : Boolean;
      Out_Elem   : Queue_Element;

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
         Process(Dis_Object.all, Res, Control);
      end loop;

      Get(Dis_Object.all, Out_Elem);
      Pac2 := Out_Elem.Binary_Payload;

   if Buffer(Binary_Packet(Pac2)) = Data.all then
      return True;
      else
         return False;
      end if;
   end Process_Test;




   ------- FUNCTION FORCE_TEST -----------
      function Force_Test (X : Entangler_Case) return Boolean is
      Time : Timestamp_Type := X.Times;
      Pac  : Application_Packet;

      Ent_Obj   : Entangler_Pt;
      Init_Pt   : Parameters_Class_Pt;
      Res       : Entangled_Data;

      Fattore   : Integer := X.Fact;
      Elementi  : Array_1D(1 .. Fattore);
      Data      : Byte_Array_Pt;

      Dis_Object : Disentangler_Pt := New_Disentangler;
      Control	 : Boolean;
      Out_Elem 	 : Queue_Element;
      Ent_Out 	 : Entangled_Data;

   begin
     -- Creation of a vector of elements of galois --
   for K in 1 .. Fattore
     loop
      Elementi(K) := To_Galois(Integer( K ));
      end loop;
      ---------------------------------------------
      Data := X.Data;

      Pac := Create_Application_Packet (Time  , Data.all );

      Force(Dis_Object.all, X.Times, Control);

      if Control = True then
         return False;
         end if;

   for T in 1 .. 1
   loop
      -- Entangler Object --
      Ent_Obj := New_Entangler;
      Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
                                  Galois_Element => Integer(To_int(Elementi(T))));
      Set_Default(Ent_Obj.all , Init_Pt);
      Entangle(Ent_Obj.all, Pac , Res);
      -- ***************** --
        Process(Dis_Object.all, Res, Control);
      end loop;

      Force(Dis_Object.all, X.Times, Control);
      Get(Dis_Object.all, Out_Elem);
      Ent_Out := Out_Elem.Entangled_Payload;

   if Res = Ent_Out then
      return True;
      else
         return False;
      end if;
   end Force_Test;



     ------- FORGET_TEST -----------
      function Forget_Test (X : Entangler_Case) return Boolean is
      Time : Timestamp_Type := X.Times;
      Pac  : Application_Packet;

      Ent_Obj   : Entangler_Pt;
      Init_Pt   : Parameters_Class_Pt;
      Res       : Entangled_Data;

      Fattore   : Integer := X.Fact;
      Elementi  : Array_1D(1 .. Fattore);
      Data      : Byte_Array_Pt;

      Dis_Object : Disentangler_Pt := New_Disentangler;
      Control    : Boolean;

   begin
     -- Creation of a vector of elements of galois --
   for K in 1 .. Fattore
     loop
      Elementi(K) := To_Galois(Integer( K ));
      end loop;
      ---------------------------------------------
      Data := X.Data;

      Pac := Create_Application_Packet (Time  , Data.all );

      -- Verifica della coda di INPUT --
   for T in 1 .. 2
   loop
      -- Entangler Object --
      Ent_Obj := New_Entangler;
      Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
                                  Galois_Element => Integer(To_int(Elementi(T))));
      Set_Default(Ent_Obj.all , Init_Pt);
      Entangle(Ent_Obj.all, Pac , Res);
      -- ***************** --
        Process(Dis_Object.all, Res, Control);
      end loop;

      Forget(Dis_Object.all, X.Times);
      Force(Dis_Object.all, X.Times, Control);

       if Control = True then
           return False;
        end if;

        if Any_Ready(Dis_Object.all) = True then
           return False;
      end if;

      --Verifica della coda di OUTPUT --
--       for T in 1 .. Fattore
--       loop
--        -- Entangler Object --
--        Ent_Obj := New_Entangler;
--        Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
--                                    Galois_Element => Integer(To_int(Elementi(T))));
--        Set_Default(Ent_Obj.all , Init_Pt);
--        Entangle(Ent_Obj.all, Pac , Res);
--        -- ***************** --
--          Process(Dis_Object.all, Res, Control);
--        end loop;
--
--        Force(Dis_Object.all, X.Times, Control);
--        if Control = True then
--             return False;
--        end if;
--
--        if Any_Ready(Dis_Object.all) = False then
--             return False;
--        end if;
--
--        Forget(Dis_Object.all, X.Times);
--
--        if Any_Ready(Dis_Object.all) = True then
--             return False;
--        end if;

      return True;
   end Forget_Test;


        ------- FORGET_TEST_OUT -----------
      function Forget_Test_Out (X : Entangler_Case) return Boolean is
      Time : Timestamp_Type := X.Times;
      Pac  : Application_Packet;

      Ent_Obj   : Entangler_Pt;
      Init_Pt   : Parameters_Class_Pt;
      Res       : Entangled_Data;

      Fattore   : Integer := X.Fact;
      Elementi  : Array_1D(1 .. Fattore);
      Data      : Byte_Array_Pt;

      Dis_Object : Disentangler_Pt := New_Disentangler;
      Control    : Boolean;

   begin
     -- Creation of a vector of elements of galois --
   for K in 1 .. Fattore
     loop
      Elementi(K) := To_Galois(Integer( K ));
      end loop;
      ---------------------------------------------
      Data := X.Data;

      Pac := Create_Application_Packet (Time  , Data.all );

      -- Verifica della coda di INPUT --
--     for T in 1 .. 2
--     loop
--        -- Entangler Object --
--        Ent_Obj := New_Entangler;
--        Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
--                                    Galois_Element => Integer(To_int(Elementi(T))));
--        Set_Default(Ent_Obj.all , Init_Pt);
--        Entangle(Ent_Obj.all, Pac , Res);
--        -- ***************** --
--          Process(Dis_Object.all, Res, Control);
--        end loop;
--
--        Forget(Dis_Object.all, X.Times);
--        Force(Dis_Object.all, X.Times, Control);
--
--         if Control = True then
--             return False;
--          end if;
--
--          if Any_Ready(Dis_Object.all) = True then
--             return False;
--        end if;

      --Verifica della coda di OUTPUT --
     for T in 1 .. Fattore
     loop
      -- Entangler Object --
      Ent_Obj := New_Entangler;
      Init_Pt := Create_Parameter(Reduction_Factor =>Fattore,
                                  Galois_Element => Integer(To_int(Elementi(T))));
      Set_Default(Ent_Obj.all , Init_Pt);
      Entangle(Ent_Obj.all, Pac , Res);
      -- ***************** --
        Process(Dis_Object.all, Res, Control);
      end loop;

      Force(Dis_Object.all, X.Times, Control);
      if Control = True then
           return False;
      end if;

      if Any_Ready(Dis_Object.all) = False then
           return False;
      end if;

      Forget(Dis_Object.all, X.Times);

      if Any_Ready(Dis_Object.all) = True then
           return False;
      end if;

      return True;
   end Forget_Test_Out;





   procedure Test_Function_Process is
     new Do_Suite(Test_Case => Entangler_Case,
                  Test_Case_Array => Entangler_Case_Array,
                  Check => Process_Test);

   procedure Test_Function_Force is
     new Do_Suite(Test_Case => Entangler_Case,
                  Test_Case_Array => Entangler_Case_Array,
                  Check => Force_Test);

      procedure Test_Function_Forget is
     new Do_Suite(Test_Case => Entangler_Case,
                  Test_Case_Array => Entangler_Case_Array,
                  Check => Forget_Test);

         procedure Test_Function_Forget_Out is
     new Do_Suite(Test_Case => Entangler_Case,
                  Test_Case_Array => Entangler_Case_Array,
                  Check => Forget_Test_Out);


begin


   Data1.all := (1, 0);

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

      Cases_Param := ((Fact => Fattore1, Data => Data1, Times => 10),
                      (Fact => Fattore1, Data => Data2, Times => 9),
                      (Fact => Fattore1, Data => Data3, Times => 11),
                      (Fact => Fattore1, Data => Data4, Times => 101),
                      (Fact => Fattore1, Data => Data5, Times => 102),
                      (Fact => Fattore1, Data => Data6, Times => 103),
                      (Fact => Fattore2, Data => Data1, Times => 104),
                      (Fact => Fattore2, Data => Data2, Times => 110),
                      (Fact => Fattore2, Data => Data3, Times => 210),
                      (Fact => Fattore2, Data => Data4, Times => 310),
                      (Fact => Fattore2, Data => Data5, Times => 410),
                      (Fact => Fattore2, Data => Data6, Times => 510),
                      (Fact => Fattore3, Data => Data1, Times => 610),
                      (Fact => Fattore3, Data => Data2, Times => 710),
                      (Fact => Fattore3, Data => Data3, Times => 810),
                      (Fact => Fattore3, Data => Data4, Times => 120),
                      (Fact => Fattore3, Data => Data5, Times => 130),
                      (Fact => Fattore3, Data => Data6, Times => 140),
                      (Fact => Fattore4, Data => Data1, Times => 150),
                      (Fact => Fattore4, Data => Data2, Times => 160),
                      (Fact => Fattore4, Data => Data3, Times => 170),
                      (Fact => Fattore4, Data => Data4, Times => 180),
                      (Fact => Fattore4, Data => Data5, Times => 190),
                      (Fact => Fattore4, Data => Data6, Times => 20),
                      (Fact => Fattore5, Data => Data1, Times => 40),
                      (Fact => Fattore5, Data => Data2, Times => 50),
                      (Fact => Fattore5, Data => Data3, Times => 60),
                      (Fact => Fattore5, Data => Data4, Times => 70),
                      (Fact => Fattore5, Data => Data5, Times => 80),
                      (Fact => Fattore5, Data => Data6, Times => 90),
                      (Fact => Fattore6, Data => Data1, Times => 670),
                      (Fact => Fattore6, Data => Data2, Times => 560),
                      (Fact => Fattore6, Data => Data3, Times => 450),
                      (Fact => Fattore6, Data => Data4, Times => 340),
                      (Fact => Fattore6, Data => Data5, Times => 1230),
                      (Fact => Fattore6, Data => Data6, Times => 1)
                                             );

-- Test del funzionamento di PROCESS
   Test_Function_Process(Reporter , Cases_Param);

-- Test del funzionamento di FORCE
   Test_Function_Force(Reporter , Cases_Param);

-- Test di FORGET (coda di input) combinato con FORCE e ANY_READY
   Test_Function_Forget(Reporter , Cases_Param);

-- Test di FORGET (coda di output) combinato con FORCE e ANY_READY
   Test_Function_Forget_Out(Reporter , Cases_Param);

   -- OSS: si è reso necessario lo sdoppiamento del test della procedura FORCE dopo
   -- l'inserimento del controllo del timestamp con Integer_Lists di tipo TS_Lists.
   -- All'inizio della procedura "Process" viene verificato che il Timestamp dei
   -- dati in ingresso non sia presente nella lista di tipo TS_Lists: Handler.To_Be_Ignored.
   -- Se il Timestamp è presente in questa lista i dati relativi a quel Timestamp non sono più
   -- utili (sono scaduti) e il pacchetto non viene nemmeno processato.
   -- Testando le due code in un unico test (ma usando gli stessi timestamp per la prima e
   -- per la seconda) il test della coda di output dava sempre errore perché i timestamp dei
   -- pacchetti di prova erano già stati messi "all'indice" nel test della prima coda!



   Final(Reporter);

end Test_Disentangler_Vandermonde;


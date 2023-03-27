with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Disentangler_Input_Queue; 		use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;   	use Disentangler_Internal_Queue;


with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;

with Interfaces;        use Interfaces;
with Ada.Streams;   	use Ada.Streams;
with Ada.Unchecked_Conversion;


with Vandermonde_Utility;	use Vandermonde_Utility;
with Parsing_Buffers;		use Parsing_Buffers;
with Profiles.Parsers.Vandermonde;	use Profiles.Parsers.Vandermonde;
with Profiles.Parameters;		use Profiles.Parameters;

with PPETP;			use PPETP;

with Ada.Text_IO;	use Ada.Text_IO;
with Network;		use network;	-- only for debug
package body Profiles.Disentanglers.Vandermonde is

   -- ====================== --
   -- ==== DISENTANGLER ==== --
   -- ====================== --


   ----------------------
   -- Digest of Queues --
   ----------------------




   -- This function makes a search in the Input_Queue of Disentangler
   -- to find the first Entangled_Record whose Vectors has collected
   -- all the Entangled Objects needed for re-construct the original
   -- Application_Packet.
   function Find_Complete (Obj : in Disentangler)
                           return IntSeqNum_StreamID_Key is
      Location : Disentangler_Input_Queue.Cursor;
      Temp_Ent : Entangled_Record;
      No_Ready : IntSeqNum_StreamID_Key := No_IntSeqNum_StreamID_Key;
   begin
      Location := First(Obj.Input_Queue);
      Temp_Ent := Element(Location);
      if Temp_Ent.Occupation = Temp_Ent.Reduction_Factor then
         return Key(Location);
      else
         for Count in 2 .. Integer(Length(Obj.Input_Queue))
         loop
            Location := Next(Location);
            Temp_Ent := Element(Location);
            if Temp_Ent.Occupation = Temp_Ent.Reduction_Factor then
               return Key(Location);
            end if;
         end loop;
      end if;

      return No_Ready;
   end Find_Complete;


   -- This function extracts the elemento from Input_Queue of Disentangler
   -- that corresponds to the given Key of type  Integer_Sequence_Number
   procedure Extract_Element(Hand          : in out Disentangler;
                             Index         : in     IntSeqNum_StreamID_Key;
                             Vector        : in out Entangled_Array;
                             Sequence_Num  : in out PPETP.Data_Sequence_Number;
                             StreamID      : in out PPETP.Stream_ID;
                             Factor        : in out Natural) is
      Struct : Entangled_Record;
      Location : Disentangler_Input_Queue.Cursor;
   begin
      Location     := Find(Hand.Input_Queue, Index);
      Struct       := Element(Location);
      Vector       := Struct.Vectors;
      Factor       := Struct.Reduction_Factor;
      Sequence_Num := Struct.Sequence_Num;
      StreamID     := Struct.StreamID;

      Exclude(Hand.Input_Queue, Index);
   end Extract_Element;


   ----------------------
   -- New_Disentangler --
   ----------------------
   function New_Disentangler return Disentangler_Pt is
   begin
      return new Disentangler;
   end New_Disentangler;


   -------------
   -- Process --
   -------------
   procedure Process(Handler      : in out Disentangler;
                     Peer         : in     Peer_ID;
                     StreamID     : in     PPETP.Stream_ID; --* TODO *************      ************
                     Channel      : in     PPETP.Channel_ID;
                     Sequence_Num : in     PPETP.Data_Sequence_Number;
                     Packet       : in     Raw_Data) is

      Par_Buffer: Parsing_Buffer := Make_Parsing_Buffer(Packet.Data.all);
      Parser_Van : Profiles.Parsers.Vandermonde.Parser_Pt := profiles.parsers.vandermonde.New_Parser;
      Data1  : Entangled_Payload_Pt;

      Temp : Entangled_Record;
      Initial_Entangled_Record : Entangled_Record;  -- non lo modifico mai, così mantiene i valori Occupation = 0 e vectors = null per tutte le locazioni!
      Location : Disentangler_Input_Queue.Cursor;
      Complete_Vector : Entangled_Array;
      Complete_Index : IntSeqNum_StreamID_Key;

      Output_Form : Queue_Element(False);

      Temp_Factor : Natural := 0;
      Res : Entangled_Payload_Pt;
      Local_Sequence_Num : PPETP.Data_Sequence_Number := 0;


      R    : Matrix;
      Rinv : Matrix;
      M    : Matrix;
      U    : Matrix;

      Row  : Integer;
      Medium    : Byte_Array_Pt;
      Result    : Byte_Array_Pt;
      End_Value : Integer;


      Key : Peer_Parameters_Key := Peer_Parameters_Key'(Id => Peer,
                                                        Ch => Channel);


      -- FUNZIONI DI CONVERSIONE --
      function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);

      -----------------------------
      function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);

      -----------------------------
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

   -----------------------------
      function Galois_Matrix_To_Byte_Array( M : Matrix ) return Byte_Array
      is
         Col : Integer := Galois_Matrices.Size_Col(M);
         Row : Integer := Galois_Matrices.Size_Row(M);
         Temp : Galois_Array(1 .. Row);
         --*TODO controllare dovrebbe al posto di 4 * col * row dovrebbe essere il #_Byte_GF * Col *Row
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

   -----------------------------
--        function Create_Application_Packet (Moment   : PPETP.Sequence_Number;
--                                            StreamID : PPETP.Stream_ID;
--                                            Input    : Byte_Array)
--                                         return Application_Packet is
--           Packet : Application_Packet;
--        begin
--           Packet := New_Packet (Sequence_Num => Moment,
--                                 StreamID     => StreamID,
--                                 Data         => Input);
--           return Packet;
--        end Create_Application_Packet;
   -----------------------------

   begin





      -- Find the parameters corresponding to the Peer_ID, if not founded
      -- descart the packet
      if not Handler.Peer_ID_Param.Contains(Key) then
         --Put_Line("Disentangler: Not found! " & Image(Peer));
         --scarta il pacchetto
         null;
      else
         --Put_Line("Disentangler: Trovato! ");

         Parse_Data(Handler => Parser_Van.all,
                    Flags   => Packet.Flags,
                    Inline  => Packet.Inline,
                    Source  => Par_Buffer,
                    Result  => Data1);

         --*TODO Se c'è già un pacchetto con lo stesso Galois_Element, il
         --*     pacchetto è da scartare.

         -- The parameters are setted in the Disentangler, but not the Galois_Element that
         -- depend from the peer
         Vandermonde_Ent(Data1.all).Param.Reduction_Factor :=
           Vandermonde_Par(Handler.Parameters.all).Reduction_Factor;

         Vandermonde_Ent(Data1.all).Param.GF_Size :=
           Vandermonde_Par(Handler.Parameters.all).GF_Size;

         Vandermonde_Ent(Data1.all).Param.Galois_Element :=
           Handler.Peer_ID_Param.Element(Key).Galois_Element;





         --        Put_Line("Disentangler: Internal Process");
         --        Put_Line("Disentangler: Row : " & Size_Row(Vandermonde_Ent(Data.Payload.all).Galois_Data)'img);
         --        Put_Line("Disentangler: Col : " & Size_Col(Vandermonde_Ent(Data.Payload.all).Galois_Data)'img);
         --        Put_Line("Disentangler: Par.GF_Size : " & Vandermonde_Par(Vandermonde_Ent(Data.Payload.all)
         --          .Param.all).GF_Size'img);
         --        Put_Line("Disentangler: Par.RF : " & Vandermonde_Par(Vandermonde_Ent(Data.Payload.all)
         --          .Param.all).Reduction_Factor'img);


         --* TODO   ************   Si deve lavorare qua per  inserire lo Streem_ID
         declare
            Tmp_Key : IntSeqNum_StreamID_Key := (IntSeqNum   => Integer_Sequence_Number(Sequence_Num),
                                                 StreamID => StreamID);
         begin
         if Integer(Length(Handler.Input_Queue)) = 0 then

            --         Put_Line("Disentangler: Inserito in Input_Queue;  [queue size = 0]");

            Temp := Initial_Entangled_Record;

            Insert_New_Element(Temp, Data1, Sequence_Num, StreamID);

            Insert(Container => Handler.Input_Queue,
                   Key => Tmp_Key,
                   New_Item => Temp);
         else
            --         Put_Line("Disentangler: Inserito in Input_Queue;  [queue size > 0]");
            if Find(Handler.Input_Queue, Tmp_Key) = Disentangler_Input_Queue.No_Element then

               Temp := Initial_Entangled_Record;

               Insert_New_Element(Temp, Data1, Sequence_Num, StreamID);

               Insert(Container => Handler.Input_Queue,
                      Key => Tmp_Key,
                      New_Item => Temp);
            else


               Temp := Element(Handler.Input_Queue, Tmp_Key);

               Insert_New_Element(Temp, Data1, Sequence_Num, StreamID);

               Replace(Handler.Input_Queue, Tmp_Key, Temp);
            end if;
         end if;

         end;
         Complete_Index := Find_Complete(Handler);




         -- Questo IF viene percorso solo se nei Vectors della coda
         -- di ingresso c'é una matrice completa corrispondente a un determinato Timestamp.
         if Complete_Index /= No_IntSeqNum_StreamID_Key then
            declare
               Reconstr_StreamID : PPETP.Stream_ID := 0;
               QueueKey : SeqNum_StreamID_Key;
            begin

               --         Put_Line("Disentangler: Complete Index >=0; estrazione dell'elemento ");
               Extract_Element(Handler, Complete_Index, Complete_Vector,
                               Local_Sequence_Num, Reconstr_StreamID, Temp_Factor);


               -- *********** RICOSTRUZIONE DEL BLOCCO ORIGINALE *********** --
               for T in 1 .. Temp_Factor
               loop
                  Res := Complete_Vector(T);

                  --              Put_Line("Disentangler: Res size [" &
                  --                       Size_Row(Vandermonde_Ent(Res.all).Galois_Data)'img & "," &
                  --                       Size_Col(Vandermonde_Ent(Res.all).Galois_Data)'img & "]");


                  Row := Size_Row(Vandermonde_Ent(Res.all).Galois_Data);
                  if T = 1 then
                     U := Galois_Matrices.Create(Temp_Factor, Row);
                  end if;

                  for I in 1 .. Row
                  loop
                     Galois_Matrices.Set(U, T, I, Get(Vandermonde_Ent(Res.all).Galois_Data , I, 1));
                  end loop;


               end loop;

               --           Put_Line("Disentangler: Galois_matrices size : [" & Size_Row(U)'img &"," &
               --                    Size_Col(U)'img & "]");



               -- Reconstruction --
               declare
                  Elementi  : Array_1D(1 .. Temp_Factor);
               begin
                  -- Creation of the vector of elements of galois
                  -- for build Vandermonde Matrix.

                  for K in 1 .. Temp_Factor
                  loop
                     Elementi(K) := Vandermonde_Ent(Complete_Vector(K).all).Param.Galois_Element;
                     --Elementi(K) := To_Galois(Integer( K ));
                  end loop;
                  ---------------------------------------------
                  Wandermonde(R, Temp_Factor, Elementi);
               end;

               --           Put_Line("Disentangler: R size : [" & Size_Row(R)'img &"," &
               --                    Size_Col(R)'img & "]");

               Rinv := Inv(R);
               M := Rinv * U;
               --           Put_Line("Disentangler: M size : [" & Size_Row(M)'img &"," &
               --                    Size_Col(M)'img & "]");




               Medium := new Byte_Array'(Galois_Matrix_To_Byte_Array(M));




               if Vandermonde_Ent(Res.all).Padding then
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

               Free(Medium);
               --         Put_Line("Disentangler: Result'length : " & Result.all'length'img);


               -- *********** FINE RICOSTRUZIONE *********** --

               Output_Form.Sequence_Num := Local_Sequence_Num;
               Output_Form.StreamID     := Reconstr_StreamID;
               Output_Form.Data.Data    := Result;

               QueueKey.SeqNum   := Local_Sequence_Num;
               QueueKey.StreamID := Reconstr_StreamID;

               Insert(Container => Handler.Queue,
                      Key       => QueueKey,
                      New_Item  => Output_Form);

               -- Free the complete_vector of the reconstructed packet
               for i in 1.. Temp_Factor loop
                  Free( Vandermonde_Ent_Pt(Complete_Vector(i)));
               end loop;

            end;
         end if;

      end if;

      -- verrà eliminata
      Profiles.Parsers.Vandermonde.Free(Parser_Van);

   end Process;


   -----------
   -- Force --
   -----------

   procedure Force (Handler             : in out Disentangler;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID;
                    Success   :    out Boolean)

   is
      Entangled_Object : Entangled_Data;
      Position : Disentangler_Input_Queue.Cursor;
      Temp_Element : Entangled_Record;
      Req : IntSeqNum_StreamID_Key := (IntSeqNum => Integer_Sequence_Number(Requested_Seq_Num),
                                       StreamID  => Requested_Stream_Id);
      Output : Queue_Element ( True );

      QueueKey : SeqNum_StreamID_Key := (SeqNum   => Requested_Seq_Num,
                                         StreamID => Requested_Stream_Id);
   begin
      Success := False;

      if Contains ( Handler.Input_Queue, Req ) then
         --Position := Find ( Handler.Input_Queue, Req );
         Temp_Element := Element ( Handler.Input_Queue, Req );

         Entangled_Object.Sequence_Num := Temp_Element.Sequence_Num;
         Entangled_Object.Payload      := Temp_Element.Vectors( 1 );

         Output.Sequence_Num := Temp_Element.Sequence_Num;
         Output.StreamID     := Temp_Element.StreamID;
       --  Output.Forced := True;
--         Output.Data.Payload := Entangled_Object;

         Insert( Handler.Queue, QueueKey, Output);

         Exclude(Handler.Input_Queue, Req);

        -- TS_Lists.Insert(Handler.To_Be_Ignored, Requested, TS_Lists.Ignore);

         Success := True;
      end if;


   end Force;




   ------------
   -- Remove --
   ------------
   procedure Remove (Handler             : in out Disentangler;
                     Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                     Requested_Stream_Id : in     PPETP.Stream_ID) is
   begin
      null;
   end Remove;


   -----------------
   -- Set_Default --
   -----------------
   procedure Set_Default(Handler : in out Disentangler;
                         Peer    : in     Peer_ID;
                         Channel : in     PPETP.Channel_ID;
                         Param   : in     Byte_Array_Pt) is
      Buffer : Parsing_Buffer := Make_Parsing_Buffer (Param.all);
      Parser_Van : Profiles.Parsers.Vandermonde.Parser_Pt := Profiles.Parsers.Vandermonde.New_Parser;
      Parameters : Parameters_Class_Pt := new Vandermonde_Par;

      Key : Peer_Parameters_Key := Peer_Parameters_Key'(Id => Peer,
                                                        Ch => Channel);
   begin

      Parse_Parameters(Handler    => Parser_Van.all,
                       Source     => Buffer,
                       Parameters => Parameters);

      Put_Line("Disentangler: Set_Default : ");
      Put_Line("         Peer_id: " & Peer'img);
      Put_Line("         Channel: " & Channel'img);
      Put_Line("         Param: " & Image(Vandermonde_Par(Parameters.all)));

      if Handler.Peer_ID_Param.Contains(Key) then
         Handler.Peer_ID_Param.Replace(Key      => Key,
                                       New_Item => Vandermonde_Par(Parameters.all));
      else
         Handler.Peer_ID_Param.Insert(Key      => Key,
                                      New_Item => Vandermonde_Par(Parameters.all));
      end if;

      Free(Parser_Van);

      --* TODO  Si deve liberare in qualche modo!!!!!!!!!!!
      --Free(Vandermonde_Par_Pt(Parameters));

   end Set_Default;



   ------------
   -- Forget --
   ------------
   procedure Forget(Handler             : in out Disentangler;--Root_Disentangler'Class;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID) is

      Temp_Ent : Entangled_Record;
      No_Ready : Integer_Sequence_Number := -1;
      Req : IntSeqNum_StreamID_Key := (IntSeqNum => Integer_Sequence_Number(Requested_Seq_Num),
                                       StreamID  => Requested_Stream_Id);

      QueueKey : SeqNum_StreamID_Key := (SeqNum   => Requested_Seq_Num,
                                         StreamID => Requested_Stream_Id);
   begin


      if Contains ( Handler.Input_Queue, Req ) then
         Exclude( Handler.Input_Queue, Req );
      end if;

      if Contains ( Handler.Queue, QueueKey ) then
         Exclude( Handler.Queue, QueueKey );
      end if;

   end Forget;


end Profiles.Disentanglers.Vandermonde;

with Profiles.Entangled.Basic;	use Profiles.Entangled.Basic;
with Byte_Arrays;       	use Byte_Arrays;
with Ada.Streams;		use Ada.Streams;

package body profiles.disentanglers.basic is
   -- ====================== --
   -- ==== DISENTANGLER ==== --
   -- ====================== --

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
                     StreamID     : in     PPETP.Stream_ID;
                     Channel      : in     PPETP.Channel_ID;
                     Sequence_Num : in     PPETP.Data_Sequence_Number;
                     Packet       : in     Raw_Data) is

      Element : Queue_Element(False);

      Result : Byte_Array_Pt;
      Len    : Natural := Packet.data.all'length;

      Key : SeqNum_StreamID_Key := (SeqNum   => Sequence_Num,
                                    StreamID => StreamID);

   begin

      Element.Sequence_Num := Sequence_Num;
      Element.StreamID     := StreamID;
      Result := new Byte_Array(1..Stream_Element_Offset(Len));

      Result.all := Packet.Data.all;
      Element.Data.Data := Result;


      Handler.Queue.Insert(Key      => Key,
                           New_Item => Element);

   end Process;

   -----------
   -- Force --
   -----------

   procedure Force(Handler   : in out Disentangler;
                   Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                   Requested_Stream_Id : in     PPETP.Stream_ID;
                   Success             :    out Boolean)
   is
      --AS
--        use type Data_Lists.Cursor;
--        use type PPETP.Timestamp_Type;
--
--        Found : Boolean := False;
--        Position :  Data_Lists.Cursor := Handler.Queue.First;
   begin
--        while not Found and Position /= Data_Lists.No_Element loop
--           Found := (Data_Lists.Element (Position).Timestamp = Requested);
--           Position := Data_Lists.Next(Position);
--        end loop;
--
--        Success := Found;
      Success :=False;
   end Force;

   procedure Remove (Handler   : in out Disentangler;
                     Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                     Requested_Stream_Id : in     PPETP.Stream_ID)
   is
   begin
      null;
   end Remove;



end profiles.disentanglers.basic;

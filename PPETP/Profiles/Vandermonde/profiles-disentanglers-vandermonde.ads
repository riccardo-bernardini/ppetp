with Disentangler_Input_Queue; 		use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;   	use Disentangler_Internal_Queue;
with Profiles.Parameters.Vandermonde;	use Profiles.Parameters.Vandermonde;
with Profiles.Disentanglers.Id_Parameters_Map;

package  Profiles.Disentanglers.Vandermonde is
   -- ================ --
   -- = Disentangler = --
   -- ================ --

   type Disentangler is new Root_Disentangler with private;
   type Disentangler_Pt is access Disentangler;

   function New_Disentangler return Disentangler_Pt;


   -- Set the parameters for a peer; if the peer is yet present
   -- overwrite them
   procedure Set_Default(Handler : in out Disentangler;
                         Peer    : in     Peer_ID;
                         Channel : in     PPETP.Channel_ID;
                         Param   : in     Byte_Array_Pt);


   procedure Process(Handler      : in out Disentangler;
                     Peer         : in     Peer_ID;
                     StreamID     : in     PPETP.Stream_ID;
                     Channel      : in     PPETP.Channel_ID;
                     Sequence_Num : in     PPETP.Data_Sequence_Number;
                     Packet       : in     Raw_Data);

   -- This procedure accepts some profile data and process them.
   -- If there are some packets ready to be read, set Ready to
   -- true.

   procedure Force (Handler             : in out Disentangler;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID;
                    Success             :    out Boolean);
   -- This procedure requests the reconstruction of the
   -- packet with Sequence_Num Requested.  If successfull,
   -- Success is set to True.

   procedure Remove (Handler             : in out Disentangler;
                     Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                     Requested_Stream_Id : in     PPETP.Stream_ID);

   procedure Forget(Handler             : in out Disentangler;--Root_Disentangler'Class;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID) ;


private
   type Disentangler is new Root_Disentangler with
      record
         Input_Queue   : Disentangler_Input_Queue.Map;
         Peer_ID_Param : Profiles.Disentanglers.Id_Parameters_Map.Map;
      end record;
end Profiles.Disentanglers.Vandermonde;

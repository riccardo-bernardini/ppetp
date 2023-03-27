with PPETP;				use ppetp;
with Packets.Binary.Application;    use Packets.Binary.Application;
with Integer_Lists;

with Packets.Protocol.Data;         use Packets.Protocol.Data;

with byte_arrays;		use byte_arrays;

with Disentangler_Internal_Queue;   use Disentangler_Internal_Queue;

with Profiles.Entangled;	use Profiles.Entangled;
with Profiles.Parameters;	use Profiles.Parameters;
with Profiles_Utility;		use Profiles_Utility;



with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Profiles.Disentanglers is
   -- ================== --
   -- == Disentangler == --
   -- ================== --

   type Root_Disentangler is abstract new Root_Profile_Handler with private;
   type Disentangler_Class_Pt is access all Root_Disentangler'Class;
   type Disentangler_Table is array (Profile_Type) of Disentangler_Class_Pt;


   function New_Disentangler(Profile : in Profile_Type;
                             Parameters : in Parameters_Class_Pt)
                             return    Disentangler_Class_Pt;

 --  function New_Disentangler_Table return Disentangler_Table;

   procedure Process
     (Handler      : in out Root_Disentangler;
      Peer         : in     Peer_ID;
      StreamID     : in     PPETP.Stream_ID;
      Channel      : in     PPETP.Channel_ID;
      Sequence_Num : in     PPETP.Data_Sequence_Number;
      Packet       : in     Raw_Data)
   is abstract;
   -- This procedure accepts some profile data and process them.
   -- If there are some packets ready to be read, set Ready to
   -- true.

   --procedure Process
    -- (Handler : in out Root_Disentangler'Class;
     -- Data    : in Entangled_Data);

   procedure Force(Handler   : in out Root_Disentangler;
                   Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                   Requested_Stream_Id : in     PPETP.Stream_ID;
                   Success             :    out Boolean)
   is abstract;
   -- This procedure requests the reconstruction of the
   -- packet with timestamp Requested.  If successfull,
   -- Success is set to True.

   procedure Get
     (Handler : in out Root_Disentangler;
      Result  : out Queue_Element);
   -- Extract the first packet from the queue of ready
   -- packets.  The packet can be both a "true" packet
   -- (written in Packet) or a profile data (written
   -- in Data).  The user can know which type of data was
   -- written by querying Which.  It raises Empty_Queue
   -- if no ready packets are present.

   function Any_Ready (Handler : in Root_Disentangler) return Boolean;

   procedure Forget(Handler   : in out Root_Disentangler'Class;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID)
   is abstract;  -- < AS

   procedure Remove(Handler   : in out Root_Disentangler;
                    Requested_Seq_Num   : in     PPETP.Data_Sequence_Number;
                    Requested_Stream_Id : in     PPETP.Stream_ID)

   is abstract;


   procedure Set_Default(Handler : in out Root_Disentangler;
                         Peer    : in     Peer_ID;
                         Channel : in     PPETP.Channel_ID;
                         Param   : in     Byte_Array_Pt) is abstract;


private

   type Root_Disentangler is abstract
     new Root_Profile_Handler with
      record
         Queue : Disentangler_Internal_Queue.Map;  --AS
         Parameters : Parameters_Class_Pt;
      end record;
end Profiles.Disentanglers;

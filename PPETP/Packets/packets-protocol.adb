with Ada.Text_Io;        use Ada.Text_Io;

with Profiles;           use Profiles;

with Profiles.Parameters.Basic;
with Packets.Protocol.Command;

package body Packets.Protocol is
   use PPETP;


   procedure Print (A : Protocol_Packet) is
   begin
      --Put_Line ("Profile   : " & Profile_Type'Image (A.Profile));
      Put_Line ("Address      : " & Image (A.Address));
   end Print;



--     -------------------
--     -- Make_Set_Port --
--     -------------------
--
--     function Make_Set_Port (Port    : Network.Port_Type;
--                             Profile : Profile_Type)
--                               return Command.Control_Packet is
--     begin
--        return (Profile      => Profile,
--                Sequence_Num => <>,
--                Address      => <>,
--                Command      => Command.Hello,
--                Reply_Port   => Port);
--     end Make_Set_Port;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Packet : Network_Packet)
                        return Boolean is
   begin
        return Command.Is_Command(Packet);
   end Is_Command;




end Packets.Protocol;

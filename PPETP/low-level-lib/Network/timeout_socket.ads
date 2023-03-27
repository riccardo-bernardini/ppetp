with Network;         use Network;
with Ada.Streams;     use Ada.Streams;
with Ada.Calendar;
package Timeout_Socket is
   procedure Read_With_Timeout (Socket_Ext:       in     Socket_Type;
                                Socket_Int:       in     Socket_Type := No_Socket;
                                From      :          out Sock_Addr_Type; -- Address of the sender
                                Receiving_Socket :   out Socket_Type; -- Which socket received the packet
                                Item      :          out Stream_Element_Array;
                                Last      :          out Stream_Element_Offset;
                                Deadline  :       in     Ada.Calendar.Time;
                                Status    :          out Selector_Status);
end Timeout_Socket;

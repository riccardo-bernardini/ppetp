with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Timeout_Socket is

   Selector   : Selector_Type;


   procedure Read_With_Timeout (Socket_Ext:       in     Socket_Type;
                                Socket_Int:       in     Socket_Type := No_Socket;
                                From      :          out Sock_Addr_Type; -- Address of the sender
                                Receiving_Socket :   out Socket_Type; -- Which socket received the packet
                                Item      :          out Stream_Element_Array;
                                Last      :          out Stream_Element_Offset;
                                Deadline  :       in     Ada.Calendar.Time;
                                Status    :          out Selector_Status) is
      use type Ada.Calendar.Time;

      R_Sock_Set : Socket_Set_Type;
      W_Sock_Set : Socket_Set_Type;
      --Selector   : Selector_Type;

      Wait_For   : Duration := Deadline - Ada.Calendar.Clock;
   begin
      if (Wait_For <= 0.0) then
         Status := Expired;
         Last := Stream_Element_Offset(0);
         return;
      end if;


      -- Set
      Set (R_Sock_Set, Socket_Ext);
      if Socket_Int /= No_Socket then
         Set (R_Sock_Set, Socket_Int);
      end if;


      Check_Selector (Selector, R_Sock_Set, W_Sock_Set,
                      Status,  Wait_For);


      case Status is
         when Completed =>
            declare
               Sock_Recv : Socket_Type;
            begin

               Get(R_Sock_Set, Sock_Recv);

               Receiving_Socket := Sock_Recv;

               Receive_Socket (Socket =>  Sock_Recv,
                               From   => From,
                               Item   => Item,
                               Last   => Last);
            end;
         when Expired | Aborted =>
            null;
      end case;

      empty(R_Sock_Set);
      empty(W_Sock_Set);

     -- close_selector(Selector);
   exception
      when e: others =>
         Put_Line("timeout_socket");
         Put_line(Exception_Information(e));
   end Read_With_Timeout;

begin


   Create_Selector (Selector);

end Timeout_Socket;

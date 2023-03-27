--  Package per permettere al server di distribuire i file ai vari Client.

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Send_To_Socket;

with Ada.Containers.Vectors;

with Ada.Containers.Doubly_Linked_Lists;

package Send_String is

   task type Contents is

      entry Turn_On(Poly    : in Integer;
                    Port    : in Port_Type;
                    Address : in Inet_Addr_Type);
      entry Turn_Off(Poly    : in Integer;
                     Port    : in Port_Type;
                     Address : in Inet_Addr_Type);

   end Contents;

end Send_String;



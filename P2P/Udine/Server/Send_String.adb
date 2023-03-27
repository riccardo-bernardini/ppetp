with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Send_To_Socket;

with Ada.Containers.Vectors;

with Ada.Containers.Doubly_Linked_Lists;

package body Send_String is


   task body Contents is

      type Poly_Array is record
         N_Poly    : Integer;
         N_Port    : Port_Type;
         N_Address : Inet_Addr_Type;
      end record;

      type String_Array is array(0..2) of Unbounded_String;

      package Poly_List is
        new Ada.Containers.Doubly_Linked_Lists(Element_Type => Poly_Array);

      use Poly_List;

      Buffer  : Poly_List.List;
      C       : Poly_List.Cursor;
      Flusso  : constant String := "Prova per la trasmissione dei flussi tra i vari Client";
      Count   : Integer := 0;
      Char    : Character;
      Packets : String_Array;

      procedure Send_Component(Position : in Cursor) is
            -- send delle componenti polifase
            Destination : Poly_Array;
         begin

            Destination := Element(Position);
             -- stringa da spedire al client
         Send_To_Socket(Packets(Destination.N_Poly),
                                             (Family => Family_Inet,
                                              Addr   => Destination.N_Address,
                                              Port   => Destination.N_Port));

      end Send_Component;

   begin

      loop
         select
            accept Turn_On(Poly    : in Integer;
                           Port    : in Port_Type;
                           Address : in Inet_Addr_Type) do

               -- Qui devo inserire i valori nella List
               Buffer.Append((N_Poly    => Poly,
                              N_Port    => Port,
                              N_Address => Address));

            end Turn_ON;


         or
              accept Turn_Off(Poly    : in Integer;
                              Port    : in Port_Type;
                              Address : in Inet_Addr_Type) do


               -- Qui devo togliere i valori nella List

               C := Buffer.Find((N_Poly    => Poly,
                                 N_Port    => Port,
                                 N_Address => Address));

               if C /= No_Element then

                  Buffer.Delete(C);

               end if;

              end Turn_Off;

         or
              delay 0.1;

               --  Da qui in poi invio le componenti polifase sulla porta UDP
               --  degli altri client.
               --  Mi conviene fare un loop per fare questa cosa.

            for I in Packets'Range loop
               --  Indice del carattere da spedire come i-sima componente polifase

               Char := Flusso((Count + I) mod Flusso'Length + 1);

               --  Costruire della stringa completa: Count / 3, Integer'Image(I), Char

               Packets(I) := To_Unbounded_String(Integer'Image(Count / 3)) &
                             To_Unbounded_String(" ") &
                             To_Unbounded_String(Integer'Image(I)) &
                             To_Unbounded_String(" ") &
                             To_Unbounded_String(Integer'Image(Character'Pos(Char)));
            end loop;

            Buffer.Iterate(Send_Component'Access);

            --  Incremento Count di 3 per selezionare le varie componenti polifase

            Count := Count+3;

         end select;

      end loop;




   end Contents;

end Send_String;

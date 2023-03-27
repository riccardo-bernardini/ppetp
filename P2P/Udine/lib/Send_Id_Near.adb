--                  -*- Mode: Ada -*-
--  Filename        : Send_Id_Near.adb
--  Description     : Send Id to a near peer
--
--  Procedure that verifies the presence of possible near client
--  with which he can exchange then various data.
--
--  To do this will be done a cycle characterized as it follows:
--
--  In the first part of the cycle I verify the existence of the node n + e(k),
--  where e(k) is the usual vector of zeros anywhere except in k.
--  In case affirmative I send a message to such node,
--  otherwise I continue with the loop.
--
--  In the second part of the loop, if I am in this point it means that
--  the node n + e(k) is not present. In this case I verify the existence
--  of the node n - e(k). If such node exists I will send a message type SEND,
--  otherwise I go out completely from the cycle.


with Ada.Text_IO;
use  Ada.Text_IO;

with Tokenize;
use  Tokenize;

with Ada.Exceptions;
use  Ada.Exceptions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Hash_Auth;
use  Hash_Auth;

with Id_Counters;
use  Id_Counters;

with MD5_Function;

with Generic_Make_Id;

with Send_Command;

with Actual_Make_Id;

with Hash_Id;
use  Hash_Id;

with Globals;
use  Globals;

with Send_To_Id;

procedure Send_Id_Near(Input : in Id; Dim : Positive) is
   Id_New : Id(1..Dim);
   Output : Unbounded_String;
   A      : Id_Data;
begin

   for I in reverse 1..Dim loop

      --  Prima parte del ciclo in cui verifico l'esistenza del nodo n + e(k),
      --  dove e(k) è il solito vettore di zeri ovunque eccetto in k.
      --  In caso affermativo spedisco un messaggio a tale nodo, atrimenti
      --  proseguo con il loop.

      Id_New := Input;
      Id_New(I) := Input(I) + 1;
      Output := Actual_Make_Id.Id_To_UString(Id_New);
      Put_Line(To_String(Output));
      A := Process_Id(Output);
      if A /= No_Id then
         Send_To_Id(A);
         Sender.Turn_Off(I, A.UDP_Control, A.Address); -- aggiungi indirizzo;
      end if;



      --  Se mi trovo in questo punto significa che il nodo n + e(k) non è
      --  presente. In questo caso verifico l'esstenza del nodo n - e(k).
      --  Se tale nodo esiste invierò un messaggio di tipo SEND,
      --  altrimenti esco completamente dal ciclo.

      if Id_New(I) >= 2 then
         Id_New(I) := Id_New(I) - 2;

         Output := Actual_Make_Id.Id_To_UString(Id_New);
         Put_Line(To_String(Output));
         A := Process_Id(Output);
         if A /= No_Id then
            Send_To_Id(A);
            Sender.Turn_Off(I, A.UDP_Control, A.Address);
         end if;

      end if;

   end loop;



end Send_Id_Near;



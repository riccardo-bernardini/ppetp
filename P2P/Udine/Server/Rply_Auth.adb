--                  -*- Mode: Ada -*-
--  Filename        : Rply_Auth.ads
--  Description     : Rply Auth
--
--  Procedure Rply_Auth managed by the server for the verification
--  of the authentication of the client through a mechanism of challenge.
--
--  Verified the authentication him it proceeds with the increase of the Id
--  of the Client and the sending to the Client of the respective String of ID.
--
--  It follows a verification of the presence of near nodes to which
--  eventually to send determined data.

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

--  with Generic_Make_Id;

with Send_Command;

with Actual_Make_Id;

with Hash_Id;
use  Hash_Id;

with Globals;
use  Globals;

with Send_To_Id;

with Send_Id_Near;

procedure Rply_Auth(Timestamp  : in Unbounded_String;
                    Parameters : in Token_Array) is


   Group      : constant String  := "127.0.0.1";
   Dim        : constant Natural := 2;
   Q          : Auth_Data;
   MD5        : String(1..32);
   Total      : String(1..17);
   C_Port     : String(1..5);
   C_Port_Nat : Port_Type;
   Failed     : exception;
   Id_Ustring : Unbounded_String;
   UString1   : Unbounded_String;
   Ustring2   : Unbounded_String;
   UString1_D : Unbounded_String;
   UString2_D : Unbounded_String;
--     A, B, C, D : Id_Data;

begin

   Q := Process(Timestamp);

   Total := To_String(Parameters(Parameters'First + 2)) & " "
            & To_String(Q.Sfida) & " " & "CIAO";
   MD5_Function(Total, MD5);

   --  Verifico se il codice MD5 generato corrisponde con la sfida cifrata
   --  generata in precedenza. In caso affermativo si può ritenere conclusa
   --  la fase di autenticazione.

   if MD5 /= To_String(Parameters(Parameters'First + 3)) then
      raise Failed with "Wrong Timestamp";
   else

      C_Port     := To_String(Q.UDP_Control);
      C_Port_Nat := Port_Type'Value(C_Port);


      --  A questo punto chiamo la funzione che mi calcola l'ID del client.

      declare
         Id_Client : Id := Actual_Make_Id.Make_Id_Current;
      begin

         Id_UString := Actual_Make_Id.Id_To_UString(Id_Client);
         Put_Line(To_String(Id_Ustring));

         Send_Command("ID", (1 => To_Unbounded_String(Integer'Image(Dim)),
                             2 => Id_UString),
                                                 (Family => Family_Inet,
                                                  Addr   => Inet_Addr(Group),
                                                  Port   => C_Port_Nat));

      --  A questo punto ho concluso l'autenticazione, e quindi posso procedere
      --  con la registrazione dell'Id nella rispettiva tabella.

         Put_Line(To_String(Id_Ustring));

         Register_Id(Id_UString, (Address     => Inet_Addr("127.0.0.1"),
                                   --Address     => Any_Inet_Addr,
           			   UDP_Data    => Port_Type'Value(To_String(Q.UDP_Data)),
                                   UDP_Control => C_Port_Nat,
                                   N_Flusso    => 5));

         for I in 0..2 loop
            Sender.Turn_On(I, C_Port_Nat, Inet_Addr("127.0.0.1"));
         end loop;




      --  Ora dovrò verificare se esistono i nodi vicini a quello registrato
      --  e in tal caso invierò il flusso desiderato.

         Send_Id_Near(Id_Client, Dim);


--           UString1 := Id_Near_1(Id_Client, Dim);
--
--           Put_Line(To_String(UString1));
--
--           A := Process_Id(UString1);
--
--           --  Se a questo punto esistono dei nodi vicini dovrò spedir loro il
--           --  messaggio di SEND, con i parametri ricavati da A.
--
--           if A /= No_Id then
--              Send_To_Id(A);
--           end if;
--           UString2 := Id_Near_2(Id_Client, Dim);
--
--           Put_Line(To_String(UString2));
--
--           B := Process_Id(UString2);
--
--           if B /= No_Id then
--              Send_To_Id(B);
--           end if;
--
--           UString1_D := Id_Near_1_D(Id_Client, Dim);
--
--           Put_Line(To_String(UString1_D));
--
--           C := Process_Id(UString1_D);
--
--           if C /= No_Id then
--              Send_To_Id(C);
--           end if;
--
--           UString2_D := Id_Near_2_D(Id_Client, Dim);
--
--           Put_Line(To_String(UString2_D));
--
--           D := Process_Id(UString2_D);
--
--           if D /= No_Id then
--              Send_To_Id(D);
--           end if;
      end;


   end if;

   Put_Line("Fine procedura Rply_Auth");

end Rply_Auth;


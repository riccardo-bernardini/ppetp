--  Funzione Rply_Auth.

with Ada.Text_IO;
use Ada.Text_IO;

with Tokenize;
use  Tokenize;

with Parsers;
use  Parsers;

with Ada.Exceptions;
use  Ada.Exceptions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Interfaces;
use  Interfaces;

with Listen_To_Port;

with Timestamp_Function;

with Create_CRC32;

with To_Hex;

with Send_To_Socket;

with MD5_Function;

with Make_Command;
use  Make_Command;

with Send_Command;

procedure Rply_List is

   Group : constant String := "127.0.0.1";

begin
   Send_Command("LIST", (1 => To_Unbounded_String("")),
                                                    (Family => Family_Inet,
                                                     Addr   => Inet_Addr(Group),
                                                     Port   => 55507));
end Rply_List;

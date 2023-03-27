--                  -*- Mode: Ada -*-
--  Filename        : Process_Function.ads
--  Description     : Process Function
--
--  Package for the management of the Callback functions.
--
--  To every present command in the communication protocol,
--  will correspond a Callback function, whose elaboration will depend
--  on the type of Command.
--
--  Now will be analyzed in detail 4 callback function.
--
--                    ******************
--                    ** Strt_Process **
--                    ******************
--
--  In this procedure, the first thing to be done consists of inserting in a
--  table of type Auth_Hash the values of the received timestamp
--  and the various fields that completes the array the Auth_Array.
--
--  Subsequently, after having individualized the number of the UDP port,
--  he sends the String of AUTH to the Client.
--
--                    ******************
--                    ** Auth_Process **
--                    ******************
--
--  The first step consists in the creation of a String of challenge
--  constituted by the followings fields:
--
--               Username | Challenge | Password
--
--  The field "Challenge" I draw it to me from the list of the parameters
--  that has passed me.
--
--  After a phase of elaboration in which I create a code MD5,
--  dispatch the String RPLY toward the Server.
--
--                    ******************
--                    ** Rply_Process **
--                    ******************
--
--  The function Rply_Process can be called in the followings 3 cases:
--
--  1) Answered to a request of authentication (AUTH);
--  2) Answered to a request of a list of flows (LIST);
--  3) Answered to a request of presentation among nodes (HELO).
--
--  Therefore, the first step to be done is to discriminate
--  the three functions to perform, in base to the command received.
--
--  I must create 3 external functions that will opportunely
--  manage the answer to forward to the nodes.
--
--                    ******************
--                    ** Play_Process **
--                    ******************
--
--  The aim of this procedure is to send the message of START to the module P2P.
--



with Ada.Text_IO;
use  Ada.Text_IO;

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

with Hash_Auth;
use  Hash_Auth;

with Hash_Id;
use  Hash_Id;

with Globals;
use  Globals;

with Listen_To_Port;

with Create_CRC32;

with To_Hex;

with Send_To_Socket;

with MD5_Function;

with Make_Command;
use  Make_Command;

with Make_Timestamp;
use  Make_Timestamp;

with URL;
use  URL;

with Query_State;
use  Query_State;

with Send_Command;


package Process_Function is

   procedure Auth_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array);
   procedure Play_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array);
   procedure   ID_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array);
   procedure Send_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array);

end Process_Function;




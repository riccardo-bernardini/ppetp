--                              -*- Mode: Ada -*-
--  Filename        : commands.ads
--  Description     : Commands sender/receiver
--  Author          : Roberto Cesco Fabbro
--  Created On      : 30, Gen 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : development

-- This package provide funtionalities for sending and receiving PPETP messages
-- it check if the format of the messages is correct

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Interfaces; use Interfaces;

with udp_rx_tx; use udp_rx_tx;
with Tokenize; use Tokenize;

package Commands is

   -- Commands
   type Command_Type_Set is new Unsigned_8;

   PLAY:    constant Command_Type_Set;
   HELO:    constant Command_Type_Set;
   ACK:     constant Command_Type_Set;
   PUNCH:   constant Command_Type_Set;
   UNKNOWN: constant Command_Type_Set;


   -- Overloaded functions
   function "or"(L,R: Command_Type_Set) return Command_Type_Set;

   function "and"(L,R: Command_Type_Set) return Command_Type_Set;

   function "xor"(L,R: Command_Type_Set) return Command_Type_Set;

   -- Verify if an element is in a set; if it's in, return true else return false
   --	Element:	Element to be checked
   --	Set:		Set of elements
   function In_Set(Element: in Command_Type_Set;
                   Set: in Command_Type_Set) return boolean;

   -- Return a Comman_Type_Set from a string. Are only defined for the constants
   -- defined above, in the other case raise a CONSTRAINT_ERROR exception
   function Value(Str: String) return Command_Type_Set;


   -- To use when a command don't use parameters
   NO_PARAMETERS: constant Unbounded_String;

   -- Exceptions
   command_format_error: exception;
   command_type_not_match: exception;
   command_not_yet_implemented: exception;
   command_wrong_sender: exception;

   -- This procedure premit to send a command to a peer; it check if the format
   -- is correct; if not it raise an command_format_error exception.
   --	To:		Address of the recipient of the message
   --	Command:	Command to send
   --	Parameters:	List of the parameters
   --	From:		Address of the sender
   procedure Send_Command(To: in Sock_Addr_Type;
                          Command: in Command_Type_Set;
                          Parameters: in Unbounded_String := NO_PARAMETERS;
                          From: in Sock_Addr_Type);

   -- This procedure premit to receive a command from a peer; it check if the format
   -- is correct; if not it raise an command_format_error exception
   -- if the command to be received is unknown use the the parameters Command => UNKNOWN;
   -- if the peer address from which the command is been send is to be checked,
   -- specify it in the From parameters. If a check fail raise the corrispondent
   -- exception.
   --	Listen_Addr:	Address where listen for messages
   --	Commands:	Waited commands (more commands can be OR-ed together,
   --					 no check!)
   --	Parameters:	in -> Parameters desidered (to be checked);
   --			out -> Parameters recieved when NO_PARAMETER is passed as input
   --	From:		in -> Address of the sender (to be checked);
   --			out -> Sender Address when a No_Sock_Addr is passed as input
   procedure Wait_For_Command(Listen_Addr: in Sock_Addr_Type;
                              Commands: in Command_Type_Set;
                              Parameters: in out Unbounded_String;
                              From: in out Sock_Addr_Type);

private

   -- Constants

   PLAY:    constant Command_Type_Set := 1;
   HELO:    constant Command_Type_Set := 2;
   ACK:     constant Command_Type_Set := 4;
   PUNCH:   constant Command_Type_Set := 8;
   UNKNOWN: constant Command_Type_Set := 16;

   NO_PARAMETERS: constant Unbounded_String := To_Unbounded_String("***NO_PARAMETERS***");

end Commands;

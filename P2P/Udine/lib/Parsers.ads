--                  -*- Mode: Ada -*-
--  Filename        : Parsers.adb
--  Description     : Parser
--
--  Package for the identification of the commands for
--  the protocol of communication.
--
--  It is mainly constituted by two procedures: Register and Process.
--
--  Register allows to insert the various commands and the relative
--  Callback functions in the object Parser, avoiding to insert
--  once the command more than.
--
--  Process has the purpose to look for the command identified
--  by the String "Command_Line" in the Command_Table.
--
--  N.B: The Key type can be only also a String, in how much
--       the command is constituted alone 4 characters.
--
--  N.B:  Key_Type represents the command, while Element_Type
--        the callback function.

with Ada.Containers.Vectors;

with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded.Hash;
use  Ada.Strings.Unbounded;

with Tokenize;
use  Tokenize;

with Ada.Exceptions;
use  Ada.Exceptions;

package Parsers is

   type Parser is tagged private;

   Failed : exception;
   Unknown_Command        : exception;
   Wrong_Parameter_Number : exception;

   type Callback_function is
     access procedure (Name_Command : in Unbounded_String;
                       Timestamp    : in Unbounded_String;
                       Parameters   : in Token_Array);

   -- La procedura Register consente di inserire i vari comandi,
   -- e le relative funzioni di Callback nell'oggetto Parser,
   -- evitando di inserire il comando più di una volta.

   procedure Register (P        : in out Parser;
                       Command  : in Unbounded_String;
                       Callback : in Callback_Function);


   --  La funzione Process ha lo scopo di cercare il comando identificato
   --  dalla stringa "Command_Line" nella Command_Table.

   procedure Process (P            : Parser;
                      Command_Line : in Unbounded_String);


private

   package Hash_Tables is
      new Ada.Containers.Hashed_Maps(Key_Type        => Unbounded_String,
                                     Element_Type    => Callback_Function,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");
   use Hash_Tables;
   subtype Table is Hash_Tables.Map;
   use type Table;
   type Parser is tagged record
     Command_Table : Table;
   end record;
end Parsers;


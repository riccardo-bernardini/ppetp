with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Tokens;

generic
   with function Tokenize(Input : Unbounded_String)
     return Token_Array;
package Generic_Parser is
   type Parser is tagged private;

   Failed : exception;
   Unknown_Command        : exception;
   Wrong_Parameter_Number : exception;

   type Callback_Function is
     access procedure (Name_Command : in Unbounded_String;
                       Timestamp    : in Unbounded_String;
                       Parameters   : in Token_Array);

   -- La procedura Register consente di inserire i vari comandi,
   -- e le relative funzioni di Callback nell'oggetto Parser,
   -- evitando di inserire il comando più di una volta.

   procedure Register (P        : in out Parser;
                       Command  : in     Unbounded_String;
                       Callback : in     Callback_Function);


   --  La funzione Process ha lo scopo di cercare il comando identificato
   --  dalla stringa "Command_Line" nella Command_Table.

   procedure Process (P            : in     Parser;
                      Command_Line : in     Unbounded_String);


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
end Generic_Parser;

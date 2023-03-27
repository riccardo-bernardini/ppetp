--                  -*- Mode: Ada -*-
--  Filename        : Hash_Id.ads
--  Description     : Hash Id
--
--  Package for the creation of a hash table in which to memorize the IDs
--  of the clients that have contacted the server.
--
--  It is mainly constituted by one procedure and one function:
--  Register_Id and Process_Id.
--
--  The procedure Register_Id allows to insert the various IDs assigned
--  to the clients and a list of parameters (type Id_Data) in an object Id_Table.
--
--  The function Process_Id has the purpose to look for the representative
--  String the correct ID, inside the object Id_Table.

with Ada.Containers.Vectors;

with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded.Hash;
use  Ada.Strings.Unbounded;

with Ada.Exceptions;
use  Ada.Exceptions;

with GNAT.Sockets;
use  GNAT.Sockets;

with Id_Counters;
use  Id_Counters;

package Hash_Id is

   type Id_Table is tagged private;


   type Id_Data is
      record
         Address     : Inet_Addr_Type;
         UDP_Data    : Port_Type;
         UDP_Control : Port_Type;
         N_Flusso    : Natural;
      end record;

   No_Id : Id_Data := (Address     => No_Inet_Addr,
                       UDP_Data    => No_Port,
                       UDP_Control => No_Port,
                       N_Flusso    => 0);


   --  La procedura Register_Id consente di inserire i vari ID assegnati ai
   --  client ed una lista di parametri (di tipo Id_Data) in un oggetto Id_Table.

   procedure Register_Id (ID         : in Unbounded_String;
                          Parameters : in Id_Data);


   --  La funzione Process_Id ha lo scopo di cercare la stringa rappresentante
   --  l'ID corretto, all'interno dell'oggetto Id_Table.

   function Process_Id (ID : in Unbounded_String) return Id_Data;


private

   package Hash_Tables is
      new Ada.Containers.Hashed_Maps(Key_Type        => Unbounded_String,
                                     Element_Type    => Id_Data,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");
   use Hash_Tables;
   subtype Table is Hash_Tables.Map;
   use type Table;
   type Id_Table is tagged record
     Client_Table : Table;
   end record;

   T : Id_Table;

end Hash_Id;

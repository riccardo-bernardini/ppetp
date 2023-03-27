--                  -*- Mode: Ada -*-
--  Filename        : Hash_Auth.ads
--  Description     : Hash Auth
--
--  Package for the creation of a hash table to memorize that parameters
--  that will be used then for realizing the procedure of authentication.
--
--  It is mainly constituted by one procedure and one function:
--  Register and Process.
--
--  The procedure Register allows to insert the various timestamps
--  and the relative Strings of authentication correspondents
--  in the object Auth_Table.
--
--  The function Process has the purpose to found the correct String of
--  challenge in function of the received timestamp,
--  inside the object Auth_Table.

with Ada.Containers.Vectors;

with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded.Hash;
use  Ada.Strings.Unbounded;

with Ada.Exceptions;
use  Ada.Exceptions;

package Hash_Auth is

   type Auth_Table is tagged private;


   type Auth_Data is
      record
         Sfida       : Unbounded_String;
         URL	     : Unbounded_String;
         UDP_Data    : Unbounded_String;
         UDP_Control : Unbounded_String;
         N_Flussi    : Unbounded_String;
         --  Aggiungere parametro Inet_Addr
      end record;


   --  La procedura Register consente di inserire i vari timestamp
   --  e le relative stringhe di autenticazione corrispondenti nell'oggetto
   --  Auth_Table.

   procedure Register (Timestamp  : in Unbounded_String;
                       Parameters : in Auth_Data);


   --  La funzione Process ha lo scopo di cercare la stringa di sfida corretta
   --  in funzione del timestamp ricevuto, all'interno dell'oggetto Auth_Table.

   function Process (Timestamp : in Unbounded_String) return Auth_Data;


private

   package Hash_Tables is
      new Ada.Containers.Hashed_Maps(Key_Type        => Unbounded_String,
                                     Element_Type    => Auth_Data,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");
   use Hash_Tables;
   subtype Table is Hash_Tables.Map;
   use type Table;
   type Auth_Table is tagged record
     Memory_Table : Table;
   end record;

   T : Auth_Table;

end Hash_Auth;

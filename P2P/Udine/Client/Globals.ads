with Ada.Text_IO;
use  Ada.Text_IO;

with String_Queues;

with Query_State;
use  Query_State;

package Globals is

   function Return_Port return Integer;
   Buffer         : aliased String_Queues.Queue_Type;
   DB		  : DB_Connection;
   UDP_Port	  : Integer := 10_100;

private

   UDP_Data : Integer := 10_000;

end Globals;



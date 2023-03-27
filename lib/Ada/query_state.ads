with GNAT.Sockets;
use  GNAT.Sockets;

package Query_State is
   type DB_Connection is private;

   Query_Error     : exception;
   Malformed_Reply : exception;
   Malformed_Query : exception;

   procedure Connect (Connection : in out DB_Connection;
                      Port       : in     Port_Type;
                      Host       : in     String := "127.0.0.1");

   function Get (Connection : in DB_Connection;
                 Var_Name   : in String) return String;

   function Get_Int (Connection : in DB_Connection;
                     Var_Name   : in String) return Integer;


   procedure Set (Connection : in DB_Connection;
                  Var_Name   : in String;
                  Var_Value  : in String);

   procedure Set (Connection : in DB_Connection;
                  Var_Name   : in String;
                  Var_Value  : in Integer);
private
   type DB_Connection is record
      Addr : Sock_Addr_Type;
   end record;
end Query_State;

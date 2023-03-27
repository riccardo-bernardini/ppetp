--                              -*- Mode: Ada -*-
--  Filename        : query_state.ads
--  Description     : do a SET/GET query to the central DB
--  Author          : Finta Tartaruga
--  Created On      : Mon Feb 25 22:07:53 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Basic test OK (Mar 11 2008)
with GNAT.Sockets;
use  GNAT.Sockets;

--
-- The functions in this package allows one to do GET/SET
-- queries to the central DB.
--
-- Typical usage:
--
--  DB : DB_Connection;  -- Represents the connection to the DB
--
--
--  Connect(Connection => DB,     -- Open the connection to the DB
--          Port       => 12345); -- Use by default localhost
--
--  -- Get a string value
--  P2P_Protocol_Version : String  := Get(Connection => DB,
--                                        Var_Name   => "P2P.PROTO_V");
--
--  -- Get an integer value
--  P2P_Port := Get_Int (Connection => DB,
--                       Var_Name   => "P2P.COMMAND_PORT");
--
-- -- Write a string
-- Set(DB, "MY.VERSION", "1.2.3");
--
-- -- Write an integer
-- Set(DB, "MY.PORT", 555);
--

Package Query_State is
   type DB_Connection is private;

   Query_Error     : exception;
   Malformed_Reply : exception;
   Malformed_Query : exception;

   procedure Connect (Connection : in out DB_Connection;
                      Port       : in     Port_Type;
                      Host       : in     String := "127.0.0.1");

   procedure Connect (Connection : in out DB_Connection;
                      Port       : in     Positive;
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

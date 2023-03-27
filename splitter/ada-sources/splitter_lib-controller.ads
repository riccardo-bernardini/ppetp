--
-- ** What is this? **
--
-- This module implements the _controller_ used to dialogate with the
-- external control server.  The controller provides a high-level interface
-- to the control server.
--
-- The control server identifies each player by its "player ID" which is
-- a positive integer.  Every control request (with the exception of
-- a player creation request) must give to the control server the
-- player ID.
--
-- Please note that this module implements just *one* controller that is
-- built-in in the package.  See comments in stream_table for a rationale
-- about this choice.
--
-- ** What kind of actions are possible? **
--
-- The package user can
--
--        + Set the port of the control server (just once)
--        + Ask the control server to start a new player
--        + Ask the control server to kill a started player
--
-- Status: <TESTED> almost Full coverage
--

with Splitter_Lib.Players;

package Splitter_Lib.Controller is

   -- Start a new player and return the corresponding descriptor
   function Start_Player (Player_Port : Natural)
                          return Players.Player_Descriptor;

   -- Kill a running player
   function Kill_Player (ID : Players.Player_ID) return Boolean;


   -- Set the server port and try to connect.  Raises Server_Not_Found
   -- if the connection is refused
   procedure Use_Port (Port : Natural);
   Server_Not_Found : exception;
end Splitter_Lib.Controller;

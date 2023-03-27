--
-- *******************
-- ** What is this? **
-- *******************
--
-- This is a low-level library used by package Controller to communicate
-- with the Ruby controller.  The main action provided is the function
-- Send_Command that sends a string to the Ruby controller and read
-- (and parses) the answer.
--
with Ada.Strings.Unbounded;                   use Ada.Strings.Unbounded;

package Splitter_Lib.Controller.Talker is
   type Word_Array is array (Natural range <>) of Unbounded_String;

   -- Record that represents the reply from the controller
   type Reply(N_Words : Natural) is
      record
         Error_Code : Natural;
         Words      : Word_Array (1 .. N_Words);
      end record;

   -- Configure the talker by specifying the port to be used
   -- to talk to the controller
   procedure Use_Port (Port : Natural);

   -- Send a command to the controller and returns the corresponding
   -- reply.
   function Send_Command (Command   : String;
                          Parameter : Natural)
                          return Reply;

   -- Return true if the Error_Code in X denotes success
   function Ok (X : Reply) return Boolean;

   -- Similar to the function with the same name, but the reply is
   -- ignored
   procedure Send_Command (Command   : String;
                           Parameter : Natural);


end Splitter_Lib.Controller.Talker;

--                              -*- Mode: Ada -*-
--  Filename        : test_status.ads
--  Description     : Handy package for writing testing program
--  Author          : Riccardo Bernardini
--  Created On      : Tue Sep  9 08:46:46 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

--
-- This package provides a tagged type (Status_Type) and some
-- primitive operations to be used in test programs.  Typical usage
-- is as follows
--
--    * At the beginning of the test program an object of type
--      Status_Type is declared
--
--    * Before every new test New_Test method is called
--
--    * The test result is checked and registered by calling
--      procedure Fail_If.  Checking and registration can be
--      done independently by using the procedures Failure and
--      Success.
--
--    * At the end of the program a report is printed by calling
--      Final_Report.
--

package Test_Status is
   type Status_Type is tagged private;

   procedure New_Test (Status  : in out Status_Type;
                       Message : in     String);
   -- Declare that a new test is going to begin. Print to standard
   -- output string Message preceded by "Test: "

   procedure New_Test (Status : in out Status_Type);
   -- As the procedure above, but no message is printed.

   procedure Fail_If (Status    : in out Status_Type;
                      Condition : in     Boolean;
                      Message   : in     String);
   -- If Condition is True, register a failure and print string
   -- Message preceded by "FAILURE: ".  If Condition is False,
   -- register a success.


   procedure Failure (Status  : in out Status_Type;
                      Message : in     String);
   -- Register a failure and print string Message preceded by
   -- "FAILURE: ".

   procedure Success (Status  : in out Status_Type);
   -- Register a success

   procedure Final_Report (Status : Status_Type;
                           Full   : Boolean := True);
   -- Print a final report and set the exit value to Success if and
   -- only if all the tests were successful, set the exit value to
   -- Failure otherwise.

private
   type Status_Type is tagged
      record
         N_Test    : Natural := 0;
         N_Failure : Natural := 0;
      end record;
end Test_Status;

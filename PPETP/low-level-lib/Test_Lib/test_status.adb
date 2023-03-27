with Ada.Command_Line, Ada.Text_Io;
use  Ada, Ada.Text_Io;

package body Test_Status is
   procedure New_Test (status : in out Status_Type) is
   begin
      Status.N_Test := Status.N_Test + 1;
   end New_Test;

   procedure New_Test (Status  : in out Status_Type;
                       Message : in     String) is
   begin
      Put_Line ("Test: " & Message);
      New_Test(Status);
   end New_Test;

   procedure Failure (Status  : in out Status_Type;
                      Message : in     String)
   is
   begin
      Status.N_Failure := Status.N_Failure + 1;
      Put_Line ("FAILURE: " & Message);
   end Failure;

   procedure Success (Status  : in out Status_Type) is
   begin
      null;
   end Success;

   procedure Fail_If (Status    : in out Status_Type;
                      Condition : in     Boolean;
                      Message   : in     String) is
   begin
      if (Condition) then
         Failure(Status, Message);
      else
         Success(Status);
      end if;
   end Fail_If;

   procedure Final_Report (Status : Status_Type;
                           Full   : Boolean := True) is
   begin
      if (Full) then
         Put ("Passed "
                & Integer'Image(Status.N_Test - Status.N_Failure)
                & " test");

         if (Status.N_Test - Status.N_Failure > 1) then
            Put ("s");
         end if;

         Put_Line(" out of "
                    & Integer'Image(Status.N_Test)
                    & ". ");
      end if;

      if (Status.N_Failure = 0) then
         Put_Line ("SUCCESS");
         Command_Line.Set_Exit_Status (Command_Line.Success);
      else
         Put_Line ("FAILED");
         Command_Line.Set_Exit_Status (Command_Line.Failure);
      end if;
   end Final_Report;
end Test_Status;

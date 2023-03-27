with Ada.Unchecked_Deallocation;
with Ada.Text_Io;
use  Ada.Text_Io;
package body Smart_Records is
   Current_Id : Natural := 0;
   procedure Print (X : Smart_Record) is
   begin
      Put_Line ("ID="
                & Natural'Image(X.Id)
                & " counter="
                & Natural'Image(X.Counter.all));
   end Print;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Smart_Record) is
   begin
      -- Put_Line( "Called initialize id=" & Natural'Image(Current_Id));
      Object.Counter := new Natural'(1);
      Object.Id := Current_Id;
      -- Current_Id := Current_Id + 1;
      -- Print(Object);
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Smart_Record) is
   begin
      -- Put_Line( "Called adjust id=" & Natural'Image(Object.Id));
      Object.Counter.all := Object.Counter.all + 1;
      -- Print(Object);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Smart_Record) is
      procedure Free is
         new Ada.Unchecked_Deallocation(Natural, Access_Natural);
   begin
      -- Put_Line( "Called finalize id=" & Natural'Image(Object.Id));

      Object.Counter.all := Object.Counter.all - 1;
      if (Object.Counter.all = 0) then
         Destroy (Object);
         Free(Object.Counter);
      end if;
   end Finalize;

end Smart_Records;

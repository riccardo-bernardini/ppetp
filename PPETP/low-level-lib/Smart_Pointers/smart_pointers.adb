with Ada.Unchecked_Deallocation, Ada.Finalization;
with Ada.Text_Io;
use  Ada.Text_Io;

package body Smart_Pointers is
   --
   -- Rough overview of the algorithm: when a new smart pointer
   -- is requested, first we allocate a record of type Obj_Info
   -- which has (a) a field Data which points to the Object and (b) a
   -- field Count which keeps track of how many are referencing the
   -- smart pointer.  The access to the newly created Obj_Info is
   -- stored inside the Smart_Pt record which is returned to the
   -- caller.  In other words, we have something like this
   --
   --                 +--------------+
   --                 |  Count       |
   -- +---------+     +--------------+
   -- |  Pt  ---+---->|  Data  ------+-----> Usr_Obj
   -- +---------+     +--------------+
   --  Smart_Pt           Obj_Info
   --
   --

   Current_Id : Natural := 0;

   -- procedure Print(X : String) renames Put_Line;
   procedure print (x: string) is
   begin
      null;
   end Print;
   pragma Inline(Print);

   function New_Pt (X : Object_Access) return Smart_Pt is
   begin
      Current_Id := Current_Id + 1;
      Print ("New ID=" & Integer'Image(Current_Id));

      return Smart_Pt'(Ada.Finalization.Controlled with
                         Pt => new Obj_Info'(Count => 1,
                                             Data  => X),
                       ID => Current_Id);
   end New_Pt;

   function Pt (X : Smart_Pt) return Object_Access is
   begin
      return X.Pt.Data;
   end Pt;

   procedure Free is
      new Ada.Unchecked_Deallocation(Object => Obj_Info,
                                     Name   => Obj_Info_Pt);

   procedure Finalize (Object : in out Smart_Pt) is
   begin
      if (Object.Pt = null) then
         Print("Finalize <unitialized>");
         return;
      end if;
      Print("Finalize in, ID= "
              & Integer'Image(Object.ID)
              & " count = "
              & Integer'Image(Object.Pt.Count));
      Object.Pt.Count := Object.Pt.Count - 1;
      if (Object.Pt.Count = 0) then
         Print("Finalize destroy  ID= "
                 & Integer'Image(Object.ID));
         Free(Object.Pt.Data);
         Free(Object.Pt);
      else
         Print("Finalize out, ID= "
                 & Integer'Image(Object.ID)
                 & " count = "
                 & Integer'Image(Object.Pt.Count));
      end if;
   end Finalize;

   procedure Adjust   (Object : in out Smart_Pt) is
   begin
      Print("Adjust in, ID= "
              & Integer'Image(Object.ID)
              & " count = "
              & Integer'Image(Object.Pt.Count));
      Object.Pt.Count := Object.Pt.Count + 1;
      Print("Adjust out, ID= "
              & Integer'Image(Object.ID)
              & " count = "
              & Integer'Image(Object.Pt.Count));
   end Adjust;

end Smart_Pointers;


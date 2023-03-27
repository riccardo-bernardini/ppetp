with Ada.Unchecked_Deallocation;

package body Smart_Integers is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (X : in out Smart_Int) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Integer, Aint);
   begin
      Free(X.C);
   end Destroy;

   procedure Initialize (X : in out Smart_Int) is
   begin
      Initialize(Smart_Record(X));
      X.C := new Integer;
   end Initialize;
end Smart_Integers;

with Smart_Records;
use  Smart_Records;

package Smart_Integers is
   type Aint is access Integer;

   type Smart_Int is
     new Smart_Record with
      record
        C : Aint;
      end record;

   procedure Destroy(X : in out Smart_Int);

   procedure Initialize (X : in out Smart_Int);
end Smart_Integers;

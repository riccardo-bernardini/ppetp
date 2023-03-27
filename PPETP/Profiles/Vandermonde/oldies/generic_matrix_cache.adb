with Ada.Unchecked_Deallocation;

package body Generic_Matrix_Cache is

   procedure Delete_buffer is
      new Ada.Unchecked_Deallocation(Key_Type, Key_Type_Ref);

   ------------
   -- Insert --
   ------------
   function "<" (X, Y : Table_Key) return Boolean
   is
   begin
      if (X.K'Length /= Y.K'Length) then
        raise Constraint_Error;
      end if;

      for I in X.K'Range loop
         if (X.K(I) < Y.K(I)) then
            return True;
         end if;
      end loop;

      return False;
   end "<";

   function Key_To_Table_Key
     (Key : Key_Type) return Table_Key
   is
      Result : Table_Key;
   begin
      Result.K := new Key_Type(Key'Range);
      Result.K.all := Key;
      return Result;
   end Key_To_Table_Key;

   procedure Insert
     (Map      : in out Matrix_Map;
      New_Item : Element;
      Key      : Key_Type)
   is
   begin
      Map.Insert (Key => Key_To_Table_Key(Key),
                  New_Item => New_Item);
   end Insert;

   procedure Get
     (Map    : in     Matrix_Map;
      Key    : in     Key_Type;
      Found  :    out Boolean;
      Matrix :    out Element)
   is
      Pos : Matrix_Map_Cursor;
      K   : Table_Key := Key_To_Table_Key(Key);
   begin
      Pos := Map.Find (K);
      if (Pos = Ox_Maps.No_Element) then
         Found := False;
      else
         Found := True;
         Matrix := Ox_Maps.Element (Pos);
      end if;

      Delete_Buffer(K.K);
   end Get;



end Generic_Matrix_Cache;

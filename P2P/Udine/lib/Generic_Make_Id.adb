--  Funzione per la generazione dell'ID del client.

package body Generic_Make_Id is

   function Image(X : String_Array) return String is
      Result : Unbounded_String;
   begin

      for I in X'Range loop
         Result := Result & X(I);
         if (I < X'Last) then
            Result := Result;
         end if;
      end loop;

      return To_String(Result);

   end Image;

   --     function Make_Id_Current return Unbounded_String is

   function Make_Id_Current return Id is

--        X      : String_Array(1..Dim);
--        Output : Unbounded_String;

   begin


      declare
         A : Id := Current_Id(C);
      begin

--           for I in A'Range loop
--              X(I) := To_Unbounded_String(Integer'Image(A(I)));
--           end loop;

      Next(C);

      return A;

--           return To_Unbounded_String(Image(X));

      end;


   end Make_Id_Current;

   function Id_To_UString(Input : in Id) return Unbounded_String is
      X      : String_Array(1..Dim);
      Output : Unbounded_String;
   begin

      for I in Input'Range loop
         X(I) := To_Unbounded_String(Integer'Image(Input(I)));
      end loop;

      return To_Unbounded_String(Image(X));

   end Id_To_UString;




begin

   C := New_Counter(Dim);

end Generic_Make_Id;





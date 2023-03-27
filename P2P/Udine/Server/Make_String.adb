package body Make_String is

   function Image(X : String_Array) return String is
      Result : Unbounded_String;
   begin

      for I in X'Range loop
         Result := Result & X(I);
         if (I < X'Last) then
            Result := Result & " ";
         end if;
      end loop;

      return To_String(Result);

   end Image;

end Make_String;

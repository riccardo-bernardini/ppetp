function Log2(X : Positive) return Natural is
   Result : Natural;
   Tmp : Positive;
begin
   Tmp := 1;
   Result := 0;
   while (Tmp < X) loop
      Result := Result + 1;
      Tmp := Tmp * 2;
   end loop;

   return Result;
end Log2;

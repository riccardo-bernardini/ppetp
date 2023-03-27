package body Conversions is
   function To_String(A : Stream_Element_Array) return String is
      subtype Stream_Array is Stream_Element_Array(A'Range);
      subtype Stream_String is
        String (1 + Integer (A'First) .. 1 + Integer (A'Last));

        function To_String_internal is
           new Ada.Unchecked_Conversion (Stream_Array, Stream_String);

        Result : String := To_String_Internal(A);
   begin
      return Result;
   end To_String;
end Conversions;

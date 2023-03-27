
package body Profiles.Parameters.Basic is

   function New_Parameter return Parameters_Class_Pt is
   begin
      return Parameters_Class_Pt'(new Basic_Par);
   end New_Parameter;

   function Image(X : Basic_Par) return String is
   begin
      return "()";
   end Image;

   function "="(X, Y : Basic_Par) return Boolean
   is
   begin
      return True;
   end;

--     procedure Free(X : in out Basic_Par_Pt) is
--     begin
--        null;
--     end Free;

end Profiles.Parameters.Basic;

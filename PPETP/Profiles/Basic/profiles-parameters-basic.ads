package  Profiles.Parameters.Basic is

   type Basic_Par is new Root_Parameters with private;
   type Basic_Par_Pt is access Basic_Par;

   function New_Parameter return Parameters_Class_Pt;

   function Image(X : Basic_Par) return String;

   function "="(X, Y : Basic_Par) return Boolean;

--   procedure Free(X : in out Basic_Par_Pt);

private
   type Basic_Par is new Root_Parameters with null record;

end Profiles.Parameters.Basic;

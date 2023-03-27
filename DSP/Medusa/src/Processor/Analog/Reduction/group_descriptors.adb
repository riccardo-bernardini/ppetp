package body Group_Descriptors is

   ---------
   -- "<" --
   ---------

   function "<"(X, Y: Group_Name) return Boolean is
   begin
      return (X.Class < Y.Class) or else
        (X.Class = Y.Class and then X.Dim < Y.Dim);
   end "<";



   ---------
   -- "=" --
   ---------

   function "="(X, Y: Group_Name) return Boolean is
   begin
      return (X.Class = Y.Class) and (X.Dim = Y.Dim);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<"(X, Y: Pool_Name) return Boolean is
   begin
      return (X.Class < Y.Class) or else
        (X.Class = Y.Class and X.Dim < Y.Dim) or else
        (X.Class = Y.Class and X.Dim = Y.Dim and X.Pool < Y.Pool);
   end "<";



   ---------
   -- "=" --
   ---------

   function "="(X, Y: Pool_Name) return Boolean is
   begin
      return (X.Class = Y.Class)
        and (X.Dim = Y.Dim)
        and (X.Pool = Y.Pool);
   end "=";





end Group_Descriptors;

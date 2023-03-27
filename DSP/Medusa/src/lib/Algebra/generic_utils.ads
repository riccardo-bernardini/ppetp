package Generic_Utils is
   generic
      type Scalar is private;
      One: Scalar;
      with function "*"(Left, Right: Scalar) return Scalar is <>;
   function Generic_Exp(X: Scalar; Exp: Natural) return Scalar;


   generic
      type In_Type  is private;
      type Out_Type is private;
      type Idx_Type is (<>);
      type In_Vec   is array(Idx_Type range <>) of In_Type;
      type Out_Vec  is array(Idx_Type range <>) of Out_Type;
      type Mapper   is access function(X: In_Type) return Out_Type;

      function Generic_Map(X: In_Vec; F: Mapper) return Out_Vec;
end Generic_Utils;


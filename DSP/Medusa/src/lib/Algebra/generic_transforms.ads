generic
   type Coefficient is digits <>;
   type Vector is array(Integer range <>) of Coefficient;
package Generic_Transforms is
   function Dct(X : Vector) return Vector;
   function Inv_Dct(X : Vector) return Vector;

   procedure In_Place_Dct(X : in out Vector);
   procedure In_Place_Inv_Dct(X : in out Vector);
end Generic_Transforms;

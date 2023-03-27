--                              -*- Mode: Ada -*-
--  Filename        : check_field.ads
--  Description     : Generic package for checking field axioms
--  Author          : Finta Tartaruga
--  Created On      : Sat Nov 17 16:00:37 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : beta
generic
   type Field is private;

   Name : String;  -- A human description of the field
   Zero : Field;   -- Addition neutral element
   One  : Field;   -- Product  neutral element

   with function "+" (E1, E2: Field) return Field is <>;
   with function "-" (E1, E2: Field) return Field is <>;
   with function "-" (E: Field)      return Field is <>;

   with function "*" (E1, E2: Field) return Field is <>;
   with function "/" (E1, E2: Field) return Field is <>;
   with function Inv (E: Field)      return Field is <>;

   with function Image (E: Field)    return String is <>;

   type Generator is private;
   with procedure Reset (G : in out Generator) is <>;
   with procedure Next  (G : in out Generator) is <>;
   with function  Again (G : in     Generator) return Boolean is <>;
   with function  Value (G : in     Generator) return Field is <>;
package Check_Field is
   function Check return Boolean;
end Check_Field;

--                              -*- Mode: Ada -*-
--  Filename        : rationals.ads
--  Description     : Mini-rational library
--  Author          : Finta Tartaruga
--  Created On      : Fri Sep 12 22:28:48 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : test_rational.adb OK

--
-- Mini rational library written only to do test with the 
-- generic_matrix package using a field with no numerical
-- error.
--
package Rationals is
   type Rational is 
      record
	 Num : Integer;
	 Den : Positive;
      end record;
   
   function "+"(Left, Right : Rational) return Rational;
   function "-"(Left, Right : Rational) return Rational;
   function "-"(X : Rational) return Rational;
   function "*"(Left, Right : Rational) return Rational;
   function "/"(Left, Right : Rational) return Rational;
   function "/"(Left, Right : Integer) return Rational;
   function Inv(X : Rational) return Rational;
   function Is_Unit(X : Rational) return Boolean;
   function Is_Zero(X : Rational) return Boolean;
   
   function Image(X : Rational) return String;
   function To_Rational(X : Integer) return Rational;
   
   Zero : constant Rational := (Num => 0, Den => 1);
   One  : constant Rational := (Num => 1, Den => 1);
   function Simplify(X : Rational) return Rational;
   function "="(Left, Right : Rational) return Boolean;
   Division_By_Zero : exception;
   Invalid_Rational : exception;
end Rationals;

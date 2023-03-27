--                              -*- Mode: Ada -*-
--  Filename        : big_numbers.ads
--  Description     : Big Integer Library
--  Author          : Riccardo Bernardini
--  Created On      : Fri Oct 24 09:07:23 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Ada.Finalization;  use Ada.Finalization;
with System;

package Big_Numbers is

   type Big_Int is new Controlled with private;

   subtype Basis_Type is Integer range 2..16;
   subtype Sign_Type  is Integer range -1 .. 1;

   -- Constructors
   function Zero return Big_Int;
   function One  return Big_Int;
   function To_Big_Int (X : Integer) return Big_Int;
   function To_Big_Int (S     : String;
                        Basis : Positive) return Big_Int;

   function To_String (X     : Big_Int;
                       Basis : Positive) return String;

   Invalid_Number : exception;
   -- Unary sign
   function "-"   (X : Big_Int) return Big_Int;

   -- Additive operators
   function "+"   (Left, Right : Big_Int) return Big_Int;
   function "-"   (Left, Right : Big_Int) return Big_Int;

   -- Multiplicative operators
   function "*"   (Left, Right : Big_Int) return Big_Int;
   function "/"   (Left, Right : Big_Int) return Big_Int;
   function "mod" (Left, Right : Big_Int) return Big_Int;
   function "rem" (Left, Right : Big_Int) return Big_Int;
   function "abs" (X : Big_Int) return Big_Int;

   procedure Divmod (Num       : in     Big_Int;
                     Den       : in     Big_Int;
                     Quotient  :    out Big_Int;
                     Remainder :    out Big_Int);

   -- Relational operators
   function "<"   (Left, Right : Big_Int) return Boolean;
   function "<="  (Left, Right : Big_Int) return Boolean;
   function ">"   (Left, Right : Big_Int) return Boolean;
   function ">="  (Left, Right : Big_Int) return Boolean;
   function "="   (Left, Right : Big_Int) return Boolean;

   function Cmp (Left, Right : Big_Int) return Sign_Type;
   function Sign (X : Big_Int) return Sign_Type;
private
   type Bigint_Entry  is mod 2 ** 16;

   Log_2_Base : constant Positive := 8;
   Base       : constant Bigint_Entry := 2 ** Log_2_Base;

   type Bigint_Holder is array (Natural range <>) of Bigint_Entry;
   type Bigint_Holder_Pt is access Bigint_Holder;

   type Big_Int is
     new Controlled with
      record
         Sign  : Sign_Type        := 0;
         Value : Bigint_Holder_Pt := null;
      end record;

   overriding
     procedure Finalize (X : in out Big_Int);

   overriding
     procedure Adjust   (X : in out Big_Int);

   --overriding
   --  procedure Initialize (X : in out Big_Int);
end Big_Numbers;



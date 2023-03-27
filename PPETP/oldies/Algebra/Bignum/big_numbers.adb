with Ada.Unchecked_Deallocation;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
with Big_Numbers.Primitive;       use Big_Numbers.Primitive;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;

package body Big_Numbers is


   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Bigint_Holder,
                                      Name   => Bigint_Holder_Pt);

   --
   -- A Big_Number is stored as an array Value of Unsigned_16 and
   -- a sign (which can assume values -1..1).  The value of the
   -- Big_Number is
   --
   --    case Sign
   --      when 0 =>
   --         The value is zero, independtently on Value which can
   --         even be null (Value is actually stored as an access
   --         to array)
   --      when others =>
   --         Sign * sum_{k=0}^{Value'last} Value(k)*Base^k
   --    end case;
   --

   --
   -- This type is quite handy to handle all the possible sign
   -- combinations in sum/sub/product/...
   --
   type Sign_Pair_Type is (Pos_Pos,  Pos_Zero,  Pos_Neg,
                           Zero_Pos, Zero_Zero, Zero_Neg,
                           Neg_Pos,  Neg_Zero,  Neg_Neg);

   --
   -- Array handy to transform a pair of signs into the corresponding
   -- Sign_Pair_Type value.
   --
   Signs_To_Pair : constant array(Sign_Type, Sign_Type) of Sign_Pair_Type
     := (-1 => (-1 => Neg_Neg,  0 => Neg_Zero,  1 => Neg_Pos),
         0 =>  (-1 => Zero_Neg, 0 => Zero_Zero, 1 => Zero_Pos),
         1 =>  (-1 => Pos_Neg,  0 => Pos_Zero,  1 => Pos_Pos));

   --
   --
   --
   Overflow_Threshold : constant Bigint_Entry :=
     Bigint_Entry(Integer (Bigint_Entry'Modulus) / 2);


   -------------------
   -- Is_Normalized --
   -------------------

   function Is_Normalized (X : Big_Int) return Boolean is
   begin
      return (X.Sign = 0 or else X.Value(X.Value'Last) /= 0);
   end Is_Normalized;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (X : Big_Int) return Boolean is
   begin
      if (X.Value = null) then
         return False;
      end if;

      if (X.Sign = 0) then
         return True;
      end if;

      if (X.Value(X.Value'Last) = 0) then
         return False;
      end if;

      for I in X.Value'Range loop
         if (X.Value (I) >= Base) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid;

   -------------
   -- Sign_Of --
   -------------

   function Sign_Of (X : Integer) return Sign_Type is
   begin
      if (X > 0) then
         return 1;
      elsif (X = 0) then
         return 0;
      else
         return -1;
      end if;
   end Sign_Of;

   ---------------
   -- Sign_Pair --
   ---------------

   function Sign_Pair (Left : Big_Int; Right : Integer)
                      return Sign_Pair_Type is
   begin
      return Signs_To_Pair(Left.Sign, Sign_Of(Right));
   end Sign_Pair;

   ---------------
   -- Sign_Pair --
   ---------------

   function Sign_Pair (Left : Integer; Right : Big_Int)
                      return Sign_Pair_Type is
   begin
      return Signs_To_Pair(Sign_Of(Left), Right.Sign);
   end Sign_Pair;

   ---------------
   -- Sign_Pair --
   ---------------

   function Sign_Pair (Left, Right : Big_Int)
                      return Sign_Pair_Type is
   begin
      return Signs_To_Pair(Left.Sign, Right.Sign);
   end Sign_Pair;

   ------------
   -- Resize --
   ------------

   procedure Resize (X    : in out Big_Int;
                    Size : Natural) is
   begin
      if (X.Value = null or else X.Value'Length /= Size) then
         Free(X.Value);

         X.Value := new Bigint_Holder(0..Size-1);
      end if;

      pragma Assert (X.Value'Length = Size);
   end Resize;


   ----------------
   -- To_Big_Int --
   ----------------

   --
   -- Take a Bigint_Holder and a sign value and return the
   -- corresponding Big_Int in normalized form, that is,
   -- Value(Value'Last) /= 0
   --
   function To_Big_Int (X    : Bigint_Holder;
                        Sign : Sign_Type := 1) return Big_Int is
      Start : Integer;
   begin
      Start := Get_Top (X);
      pragma Assert (Start = X'Last or else X (Start + 1) = 0);

      return Result : Big_Int do
         Resize (Result, Start + 1);
         Result.Value.all := X (0 .. Start);
         Result.Sign := Sign;
      end return;
   end To_Big_Int;



   ----------------
   -- To_Big_Int --
   ----------------

   function To_Big_Int (X : Integer) return Big_Int is
      function N_Digit (X : Natural) return Positive is
         package Math is
           new Ada.Numerics.Generic_Elementary_Functions (Float);
      begin
         if (X = 0) then
            return 1;
         else
            return Positive(Float'Ceiling (Math.Log (Float(X), Float(Base))));
         end if;
      end N_Digit;

   begin
      if (X = 0) then
         return Zero;
      end if;

      declare
         Buffer : Bigint_Holder (0 .. N_Digit (X)-1);
      begin
         Set_Value (Value => abs X, Buffer => Buffer);

         if (X > 0) then
            return To_Big_Int (Buffer, 1);
         else
            return To_Big_Int (Buffer, -1);
         end if;
      end;
   end To_Big_Int;


   -------------------
   -- Ch_To_Integer --
   -------------------

   function Ch_To_Integer (C : Character;
                           B : Positive)
                           return Big_Int is
      Result : Big_Int := (Controlled with
                           Sign  => 1,
                           Value => new Bigint_Holder (0 .. 0));
      Tmp : Bigint_Entry;
   begin
      case C is
         when '0' .. '9' =>
            Tmp := Character'Pos(C) - Character'Pos('0');
         when 'a' .. 'f' =>
            Tmp := Character'Pos(C) - Character'Pos('a') + 10;
         when 'A' .. 'F' =>
            Tmp := Character'Pos(C) - Character'Pos('A') + 10;
         when others =>
            raise Invalid_Number;
      end case;

      if (Tmp >= Bigint_Entry(B)) then
         raise Invalid_Number;
      end if;

      Result.Value (0) := Tmp;

      return Result;
   end Ch_To_Integer;

   ----------------
   -- To_Big_Int --
   ----------------

   function To_Big_Int (S     : String;
                        Basis : Positive) return Big_Int is
      Sign   : Sign_Type := 1;
      Cursor : Positive  := S'First;
      Result : Big_Int   := Zero;
      B      : Big_Int   := To_Big_Int(Basis);
   begin
      while Cursor <= S'Last and then S (Cursor) = ' ' loop
         Cursor := Cursor + 1;
      end loop;

      if (Cursor <= S'Last and then S(Cursor) = '-') then
         Sign := -1;
         Cursor := Cursor + 1;
      end if;

      if (Cursor > S'Last) then
         raise Invalid_Number;
      end if;

      while (Cursor <= S'Last) loop
         Result := B * Result + Ch_To_Integer (S (Cursor), Basis);
         Cursor := Cursor + 1;
      end loop;

      Result.Sign := Sign;
      return Result;
   end To_Big_Int;

   function To_String (X     : Big_Int;
                       Basis : Positive) return String is
      To_Chr    : constant String (1 .. 16) := "0123456789abcdef";
      Result    : Unbounded_String := Null_Unbounded_String;
      Local_X   : Bigint_Holder := X.Value.all;
      Quotient  : Bigint_Holder (Local_X'Range);
      Remainder : Bigint_Holder (Local_X'Range);
      B         : Bigint_Holder (0..0) := (0 => Bigint_Entry(Basis));
   begin
      if (X.Sign = 0) then
         return "0";
      end if;

      while not Is_Zero(Local_X) loop
         Divmod (Num => Local_X,
                 Den => B,
                 Quotient  => Quotient,
                 Remainder => Remainder);

         Result  := To_Chr (Integer(Remainder(0)) + 1) & Result;
         Local_X := Quotient;
      end loop;

      if (X.Sign < 0) then
         Result := "-" & Result;
      end if;

      return To_String (Result);
   end To_String;

   ----------
   -- Zero --
   ----------

   function Zero return Big_Int is
   begin
      return Result : Big_Int do
        Result.Sign  := 0;
        Result.Value := new Bigint_Holder'((0=>0));
      end return;
   end Zero;

   ---------
   -- One --
   ---------

   function One return Big_Int is
   begin
      return Result : Big_Int do
        Result.Sign  := 1;
        Result.Value := new Bigint_Holder'((0 => 1));
      end return;
   end One;


   ----------------
   -- Actual_Sum --
   ----------------

   --
   -- Sum two positive Big_Int
   --
   function Actual_Sum (Left, Right : Big_Int) return Big_Int is
      Biggest : Natural := Natural'Max (Left.Value'Last, Right.Value'Last);
      Buf_L   : Bigint_Holder (0 .. Biggest+1);
      Result  : Bigint_Holder (0 .. Biggest+1);
   begin
      pragma Assert(Left.Sign = 1);
      pragma Assert(Right.Sign = 1);


      Extend_In (Left,  Buf_L);
      Extend_In (Right, Result);

      Add (Accumulator => Result, Operand => Buf_L);

      return To_Big_Int(Result);
   end Actual_Sum;


   ----------------
   -- Actual_Sub --
   ----------------

   --
   -- Compute the difference of two positive Big_Int
   --
   function Actual_Sub (Left, Right : Big_Int) return Big_Int is

   begin
      pragma Assert(Left.Sign = 1);
      pragma Assert(Right.Sign = 1);

      --
      -- It is more convenient if we know that the result will
      -- be positive.  If it is not, swap Right and Left and
      -- change sign
      --
      if (Left < Right) then
         return - Actual_Sub (Right, Left);
      end if;

      pragma Assert (Left.Value'Last >= Right.Value'Last);

      declare
         Result : Bigint_Holder (Left.Value'Range);
         Buf_R  : Bigint_Holder (Left.Value'Range);
      begin
         Extend_In (Src => Left,  Dst => Result);
         Extend_In (Src => Right, Dst => Buf_R);

         Sub(Result, Buf_R);

         return To_Big_Int(Result);
      end;
   end Actual_Sub;


   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Big_Int) return Big_Int is
   begin
      pragma Assert (Left.Value'First = 0);
      pragma Assert (Right.Value'First = 0);

      case Sign_Pair(Left, Right) is
         when Zero_Pos | Zero_Zero | Zero_Neg =>
            return Right;
         when Pos_Zero | Neg_Zero =>
            return Left;
         when Pos_Pos =>
            return Actual_Sum (Left, Right);
         when Neg_Neg =>
            return -Actual_Sum (-Left, -Right);
         when Pos_Neg =>
            return Actual_Sub(Left, -Right);
         when Neg_Pos =>
            return Actual_Sub(Right, -Left);
      end case;
   end "+";

   function "-" (X : Big_Int) return Big_Int is
      Result : Big_Int := X;
   begin
      pragma Assert (X.Value'First = 0);
      Result.Sign := - Result.Sign;
      return Result;
   end "-";

   function "-" (Left, Right : Big_Int) return Big_Int is
   begin
      return Left + (- Right);
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (X : Big_Int) return Big_Int is
      Result : Big_Int := X;
   begin
      pragma Assert (X.Value'First = 0);
      Result.Sign := abs Result.Sign;
      return Result;
   end "abs";

   function Actual_Prod (Left, Right : Big_Int) return Big_Int is
      Len       : Positive := Left.Value'Length + Right.Value'Length;
      Buf_Short : Bigint_Holder_Pt;
      Buf_Long  : Bigint_Holder (0 .. Len-1);
      Result    : Bigint_Holder (0 .. Len-1);
   begin
      pragma Assert (Is_Valid (Left));
      pragma Assert (Is_Valid (Right));

      if (Left.Value'Length > Right.Value'Length) then
         Extend_In (Src => Left, Dst => Buf_Long);
         Buf_Short := Right.Value;
      else
         Extend_In (Src => Right, Dst => Buf_Long);
         Buf_Short := Left.Value;
      end if;

      Mult(Result => Result,
           Long   => Buf_Long,
           Short  => Buf_Short.all);

      return To_Big_Int(Result);
   end Actual_Prod;

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Big_Int) return Big_Int is
   begin
      pragma Assert (Is_Valid(Left));
      pragma Assert (Is_Valid(Right));

      case Sign_Pair(Left, Right) is
         when Zero_Pos | Zero_Zero | Zero_Neg | Pos_Zero | Neg_Zero =>
            return Zero;
         when Pos_Pos | Neg_Neg =>
            return Actual_Prod (abs Left, abs Right);
         when Pos_Neg | Neg_Pos =>
            return - Actual_Prod (abs Left, abs Right);
      end case;
   end "*";



   ---------
   -- Cmp --
   ---------

   function Cmp (Left, Right : Big_Int) return Sign_Type is
   begin
      pragma Assert (Is_Valid (Left));
      pragma Assert (Is_Valid (Right));

      return Cmp(Left.Value.all, Right.Value.all);
   end Cmp;

   function "<" (Left, Right : Big_Int) return Boolean is
   begin
      return Cmp(Left, Right) < 0;
   end "<";

   function "<=" (Left, Right : Big_Int) return Boolean is
   begin
      return Cmp(Left, Right) <= 0;
   end "<=";

   function "=" (Left, Right : Big_Int) return Boolean is
   begin
      return Cmp(Left, Right) = 0;
   end "=";

   function ">" (Left, Right : Big_Int) return Boolean is
   begin
      return Cmp(Left, Right) > 0;
   end ">";

   function ">=" (Left, Right : Big_Int) return Boolean is
   begin
      return Cmp(Left, Right) >= 0;
   end ">=";

   function Sign (X : Big_Int) return Sign_Type
   is
   begin
      return X.Sign;
   end Sign;

   ------------------
   -- Basic_Divmod --
   ------------------

   procedure Basic_Divmod (Num       : in     Big_Int;
                           Den       : in     Big_Int;
                           Quotient  :    out Big_Int;
                           Remainder :    out Big_Int) is

   begin
      pragma Assert (Num.Sign = 1);
      pragma Assert (Den.Sign = 1);

      case Cmp(Num, Den) is
         when -1 =>
            -- Num < Den.  This is easy
            Quotient  := Zero;
            Remainder := Num;
         when 0 =>
            -- Num = Den. This is easy too
            Quotient  := One;
            Remainder := Zero;
         when 1 =>
            -- Num > Den.  No shortcut, alas...
            declare
               Q : Bigint_Holder (0..Num.value'Last - Den.Value'Last + 1);
               R : Bigint_Holder (Den.Value'Range);
            begin
               Primitive.Divmod(Num => Num.Value.all,
                                Den => Den.Value.all,
                                Quotient  => Q,
                                Remainder => R);

               Quotient  := To_Big_Int (Q);
               Remainder := To_Big_Int (R);
            end;
      end case;

      pragma Assert (Remainder < Den);
      pragma Assert (Num = Quotient * Den + Remainder);
   end Basic_Divmod;

   ------------
   -- Divmod --
   ------------

   procedure Divmod (Num       : in     Big_Int;
                     Den       : in     Big_Int;
                     Quotient  :    out Big_Int;
                     Remainder :    out Big_Int) is
   begin
      pragma Assert (Is_Valid (Num));
      pragma Assert (Is_Valid (Den));

      case Den.Sign is
         when 0 =>
           raise Constraint_Error;
         when -1 =>
            Basic_Divmod (-Num, -Den, Quotient, Remainder);
            Remainder := -Remainder;
         when 1 =>
            Basic_Divmod (Num, Den, Quotient, Remainder);
      end case;

      pragma Assert (Is_Normalized (Quotient));
      pragma Assert (Is_Normalized (Remainder));
      pragma Assert (Remainder.Sign = Den.Sign);
      pragma Assert (abs Remainder < abs Den);
      pragma Assert (Num = Quotient*Den + Remainder);
   end Divmod;

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Big_Int) return Big_Int is
      Result : Big_Int;
      Junk   : Big_Int;
   begin
      Divmod(Num       => Left,
             Den       => Right,
             Quotient  => Result,
             Remainder => Junk);

      return Result;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (Left, Right : Big_Int) return Big_Int is
      Result : Big_Int;
      Junk   : Big_Int;
   begin
      Divmod(Num       => Left,
             Den       => Right,
             Quotient  => Junk,
             Remainder => Result);

      return Result;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (Left, Right : Big_Int) return Big_Int is
   begin
      case Left.Sign is
         when 0 =>
           return Zero;
         when 1 =>
            return Left mod Right;
         when -1 =>
            return -((-Left) mod Right);
      end case;
   end "rem";

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Big_Int) is
   begin
      Free(X.Value);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust   (X : in out Big_Int) is
   begin
      if (X.Value /= null) then
         X.Value := new Bigint_Holder'(X.Value.all);
      end if;
   end Adjust;


   -- procedure Initialize (X : in out Big_Int) is
   -- begin
   --    X.Sign := 0;
   --    X.Value := new Bigint_Holder(0..0) := (0 => 0);
   -- end Initialize;
end Big_Numbers;

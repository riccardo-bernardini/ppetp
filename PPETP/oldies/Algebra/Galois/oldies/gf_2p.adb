with Text_Io; use Text_Io;
with Ada.Unchecked_Conversion;

package body Gf_2p is
   Elements_Per_Galois : constant Integer :=
     Galois'Size/Stream_Element'Size;

   package My_Io is new Text_Io.modular_Io(galois);

   --
   -- A couple of words about the implementation used.  As well known,
   -- GF(2^n) can be implemented as the set of equivalence classes of
   -- polynomials with coefficients in Z_2, modulo a n-degree polynomial
   --
   --        P(x) = x^n + R(x)
   --
   -- where R(x) represents the "tail" of P(x).  In every class there
   -- is one and only one polynomial whose degree is smaller than n.
   -- We will call such a polynomial the "standard representative" of
   -- the class.
   --
   -- We represent each element of GF(2^n) as an n-bit binary
   -- word whose k-th bit represents the coefficient of x^k of
   -- the standard representative.  With such a position
   --
   --   + Sum in GF(2^n) corresponds to the XOR of the binary words
   --
   --   + The product of A by x corresponds to a left shift of the binary
   --     word.  If after the product by x an n-degree polynomial is
   --     obtained, we add P(x) to the result, in order to obtain the
   --     "standard representative".
   --
   --   + The product of two elements A and B=\sum_k b_k x^k can be
   --     computed as
   --
   --             A*B = \sum_k b_k (x^k * A)
   --
   --     where the product x^k * A is computed as explained above.
   --     Alternatively, A*B can be computed by using a "logarithm
   --     table" (but this can be done only if the field is small)
   --
   --   + The multiplicative inverse of A can be obtained by using the
   --     Euclide algorithm to solve
   --
   --            A(x)*A_inv(x) + P(x)*Q(x) = 1
   --
   --     Alternatively, the logarithm table can be used.
   --
   --   + The division A/B can be computed as A*inv(B) or by using the
   --     logarithm table
   --

   -- Log and Exp table used for small size fields
   -- (2^n with n <= 16)
   type Log_Table_t is array (Positive range <>) of Natural;
   type Exp_Table_t is array (Natural range <>) of Galois;

   type Log_Table_Ptr is access Log_Table_t;
   type exp_Table_Ptr is access Exp_Table_t;

   Log_Table: Log_Table_Ptr;
   Exp_Table: Exp_Table_Ptr;

   type Binary_Operator is access function (Left, Right : Galois) return Galois;
   type Unary_Operator  is access function (X : Galois) return Galois;

   Product_Callback  : Binary_Operator;
   -- Division_Callback : Binary_Operator;
   Inverse_Callback  : Unary_Operator;

   --
   -- As described above, GF(2^n) is the set of equivalence classes of
   -- polynomials with coefficients in Z_2, modulo a n-degree polynomial
   --
   --        P(x) = x^n + R(x)
   --
   -- To save one bit, we do not store the binary word relative
   -- to P(x), but the one relative to R(x).  Carry_Table(n)
   -- is the binary word of R(x) for GF(2^n)
   --
   Carry_Table : array(Positive range <>) of Galois :=
     (1 => 1,   2 => 3,   3 => 3,
      4 => 3,   5 => 5,   6 => 27,
      7 => 3,   8 => 29,  9 => 17,
      10 => 2#0110_1111#,  11 => 2#0101#,
      12 => 2#1110_1011#,  13 => 2#0001_1011#,
      14 => 2#1010_1001#,  15 => 2#0011_0101#,
      16 => 2#0010_1101#,
      --
      -- Warning: GF(2^n) with n>16 still untested
      --
      17 => 2#1001#,
      18 => 2#0001_0100_0000_0011#,
      19 => 2#0010_0111#,
      20 => 2#0110_1111_0011#,
      21 => 2#0110_0101#,
      22 => 2#0001_1111_0110_0001#,
      23 => 2#0010_0001#,
      24 => 2#0001_1110_0110_1010_1001#,
      25 => 2#0001_0100_0101#,
      26 => 2#0100_0101_1101_0011#,
      27 => 2#0001_0110_1010_1101#,
      28 => 2#0010_0000_1110_0101#,
      29 => 2#0101#,
      30 => 2#0011_0010_1000_1010_1111#,
      31 => 2#1001#,
      32 => 2#1000_0010_1001_1001#,
      33 => 2#0011_1101_0100_1001#,
      34 => 2#0001_1001_1001_1111_0111#,
      35 => 2#1100_1010_0101#,
      36 => 2#1101_1010_0110_0001_0110_0011#,
      37 => 2#0011_1111#,
      38 => 2#0100_0111_0010_0111#,
      39 => 2#1001_1110_1110_0101#,
      40 => 2#1010_0101_1011_0001_0010_1011#,
      41 => 2#1001#,
      42 => 2#0100_0111_0001_0100_0001_1010_0110_0111#,
      43 => 2#0101_1001#,
      44 => 2#0001_0000_1011_0000_0000_0001_1011#,
      45 => 2#0001_0010_1101_1000_0100_0001#,
      46 => 2#1011_0010_0100_0000_0000_0001#,
      47 => 2#0010_0001#,
      48 => 2#0010_1000_0010_0001_1101_1000_1001#,
      49 => 2#0101_0101_1111#,
      50 => 2#0011_1000_0000_1011_0111_0111_0101_0101#,
      51 => 2#0001_1001_0010_0100_0001#,
      52 => 2#0001_1110_1010_0010_1100_0100_1001_0011#,
      53 => 2#0100_0111#,
      54 => 2#0101_1110_1010_0010_0111_1010_0000_1001_0111#,
      55 => 2#1110_1001_0001#,
      56 => 2#0010_0100_0100_0100_1000_0110_1011_0001_1101#,
      57 => 2#0010_1001_0010_1101_0111_1111#,
      58 => 2#1010_0111_0100_0101_0001_1101_1110_1011#,
      59 => 2#0111_1011#,
      60 => 2#0011_0110_1001_0111_0100_0110_0100_1010_0001_0001_0011_1101#,
      61 => 2#0010_0111#,
      62 => 2#0001_0111_1111_0011_1111_0111_0000_0100_0011#,
      63 => 2#0001_1100_0011_1000_1011_0001_1111#,
      64 => 2#0010_0100_0111_1111_0100_0011_1100_1011_0111#);


   Overflow_Mask : constant Galois := 2**(Exponent-1);
   Carry_Mask    : constant Galois := Carry_Table(Exponent);

   function Has_Degree (X   : Galois;
                        Deg : Natural)
                       return Boolean is
   begin
      -- Put_Line("D=" & Integer'Image(Deg));
      if (Deg >= 64) then
         -- Put_Line("---D=" & Integer'Image(Deg));
         return False;
      else
         -- Put("//=");
         -- My_Io.Put(Item => not Galois(2**Deg-1), Base => 2);
         -- Put(" !!=");
         -- My_Io.Put(Item => X and not Galois(2**Deg-1), Base => 2);
         return (X and not Galois(2**Deg-1))=2**Deg;
      end if;
   end Has_Degree;

   function Degree (X     : Galois;
                    Bound : Positive := Exponent)
                   return Natural is
      Mask   : Galois  := 2**Bound;
      Result : Natural := Bound;
   begin
      -- Put_Line("b=" & Integer'Image(Bound) & "x=" & Image(X));
      pragma Assert (X /= Zero);
      while (Mask and X) = Zero loop
         Result := Result - 1;
         Mask   := Shift_Right (Mask, 1);
      end loop;

      -- Put_Line("d=" & Integer'Image(Result));
      pragma Assert (Has_Degree (X, Result));
      return Result;
   end Degree;


   ---------------------------------
   -- Conversion from/to Integers --
   ---------------------------------

   function To_Int(X: Galois) return Interfaces.Unsigned_64 is
   begin
      return Interfaces.Unsigned_64(X);
   end To_Int;

   function To_Galois(X: Interfaces.Unsigned_64) return Galois is
   begin
      return Galois(X);
   end To_Galois;

   function To_Galois(X: Integer) return Galois is
   begin
      return Galois(X);
   end To_Galois;

   function Galois_Array_To_Stream(X : Galois_Array)
                                   return Stream_Element_Array is

      type Result_Type is
        new Stream_Element_Array(0..Stream_Element_Offset(X'Length*Elements_Per_Galois-1));

      function Conversion is
         new Ada.Unchecked_Conversion (Source => Galois_Array,
                                       Target => Result_Type);
   begin
      return Stream_Element_Array(Conversion(X));
   end Galois_Array_To_Stream;

   function Stream_To_Galois_Array (X : Stream_Element_Array)
                                   return Galois_Array
   is
      type Result_Type is
        new Galois_Array(1..X'Length/Elements_Per_Galois);

      function Conversion is
         new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
                                       Target => Result_Type);
   begin
      return Galois_Array(Conversion(X));
   end Stream_To_Galois_Array;



   -------------------------------------
   -- Sum, difference and unary minus --
   -------------------------------------

   function "+"(Left, Right: Galois) return Galois is
   begin
      return Left xor Right;
   end "+";

   function "-"(Left, Right: Galois) return Galois is
   begin
      return Left xor Right;
   end "-";

   function "-"(X: Galois) return Galois is
   begin
      return X;
   end "-";

   -------------------------------
   -- Logarithm and exponential --
   -------------------------------

   function Gf_Log(X: Galois) return Natural is
   begin
      if (X=0) then
         raise Numeric_Error;
      else
         return Log_Table(Positive(X));
      end if;
   end Gf_Log;
   pragma Inline(GF_Log);

   function Gf_Exp(X: Integer) return Galois is
   begin
      if (X not in Exp_Table'range) then
         return Exp_Table(X mod (Exp_Table'Last+1));
      else
         return Exp_Table(X);
      end if;
   end Gf_Exp;
   pragma Inline(GF_Exp);

   ----------------------------------
   -- Product via "long" algorithm --
   ----------------------------------

   --
   -- Multiply X by x.
   --
   function Gf_Single_Left_Shift (X : Galois)
                                 return Galois is
   begin
      --
      -- Multiply X by x. If the last bit of My_Left is zero,
      -- this is a simple shift, otherwise we must XOR the shift
      -- with the word representing P(x).  Note that in order to
      -- gain one bit, we do not store P(x) itself, but R(x) where
      --
      --        P(x) = x^n + R(x)
      --
      -- In order to work with this representation, we need to
      --
      --    * Clear bit n in the word to be shifted
      --    * Shift the word
      --    * XOR the result with R (stored in Carry_Mask)
      --

      if (X and Overflow_Mask) = 0 then
         return Shift_Left (X, 1);
      else
         return Shift_Left (X and not Overflow_Mask, 1) xor Carry_Mask;
      end if;
   end Gf_Single_Left_Shift;
   pragma Inline (Gf_Single_Left_Shift);

   function Gf_Left_Shift (X      : Galois;
                           Amount : Natural := 1)
                          return Galois is
      My_X : Galois := X;
   begin
      for I in 1..Amount loop
         My_X := Gf_Single_Left_Shift(My_X);
      end loop;

      return My_X;
   end Gf_Left_Shift;

   function Gf_Long_Product(Left, Right: Galois) return Galois is
      Result     : Galois := 0;
      My_Left    : Galois := Left;
      My_Right   : Galois := Right;
   begin
      -- Take care of the easy cases
      if (Left = 0 or Right = 0) then
         return 0;
      end if;

      while My_Right /= 0 loop
         --
         -- Here at the k-th iteration My_Left contains x^(k-1)*Left,
         -- while the LSB of My_Right contains the coefficient of
         -- x^(k-1) of Right.
         --
         if (My_Right and 1) /= 0 then
            Result := Result xor My_Left;
         end if;

         -- Move the coefficient of x^k of Right in the LSB
         My_Right := Shift_Right(My_Right, 1);
         My_Left  := Gf_Single_Left_Shift(My_Left);
      end loop;

      return Result;
   end Gf_Long_Product;

   function GF_Table_Product (Left, Right : Galois) return Galois is
   begin
      return Gf_Exp(Gf_Log(Left)+Gf_Log(Right));
   end GF_Table_Product;

   -------------------------------------------------
   -- Multiplicative inverse via "long" algorithm --
   -------------------------------------------------

   function Gf_Long_Inv(X: Galois) return Galois is
      type Matrix_Type is array (1..2, 1..3) of Galois;

      procedure Swap(M : in out Matrix_Type) is
         Tmp : Galois;
      begin
         for I in M'Range(2) loop
            Tmp          := M (1,I);
            M (1,I) := M (2,I);
            M (2,I) := Tmp;
         end loop;
      end;
      pragma Inline (Swap);

      procedure Combine_Rows (M     : in out Matrix_Type;
                              Shift : in     Natural) is
      begin
         for I in M'Range(2) loop
            M(2,I) := M(2,I) xor Gf_Left_Shift(M(1,I), Shift);
         end loop;
      end Combine_Rows;
      pragma Inline (Combine_Rows);

      Matrix  : Matrix_Type;
      Tmp_Deg : Positive;
      Deg     : array (1..2) of Natural;
   begin
      if (X = Zero) then
         raise Numeric_Error;
      end if;

      if (X = One) then
         return One;
      end if;

      --
      -- To compute the inverse of X we use the Euclid algorithm.
      -- More precisely, the inverse of X is the polynomial Xinv which
      -- satisfies the following equation
      --
      --          X*Xinv + P*Q = 1
      --
      -- for some polynomial Q.  In order to find suitable Xinv and Q
      -- we use the Euclid algorithm.  More precisely, we first
      -- construct matrix
      --
      --           [ X  1  0 ]
      --      M1 = [         ]
      --           [ P  0  1 ]
      --
      -- then we act over such a matrix with elementary operations
      -- (i.e., adding a row to another and swapping the rows) until
      -- we arrive to a matrix of form
      --
      --           [ 1  a  b ]
      --      M2 = [         ]
      --           [ *  c  d ]
      --
      -- It is clear that
      --
      --           [ a  b ]
      --      M2 = [      ] * M1
      --           [ c  d ]
      --
      -- It follows that
      --
      --      1 = a*X + b*P
      --
      -- that is, Xinv=a
      --

      --
      -- Now we should construct the matrix, but we are going to be a
      -- little bit subtler... If we are working in GF(2^n) polynomial
      -- P = x^n + R has degree n.  In order to gain one bit (this
      -- allows us to arrive to n=64 instead of stopping at n=63) we
      -- do not store P, but its "tail" R.
      --
      -- The Euclid algorithm would require to add to the
      -- higher-degree polynomial a multiple of the lower-degree
      -- one. Clearly, at the beginning P is the higher-degree
      -- polynomial.  We are going to delete the term in x^n of P in a
      -- "virtual" way.  If d is the degree of X we should multiply X
      -- by x^(n-d) and XOR the result with P.  To do such an
      -- operation working only with R we
      --
      --        1. Remove bit d of X (the one which would delete x^n)
      --        2. Multiply the result by n-d
      --        3. XOR with R.  Call the result C.
      --
      -- The resulting matrix is
      --
      --        [ X     1     0 ]
      --        [               ]
      --        [ C  x^(n-d)  1 ]
      --

      Deg(1) := Degree(X, Exponent);
      Deg(2) := Exponent;

      declare
         Tmp : Galois;
      begin
         Tmp := X and not (2**Deg(1));
         Tmp := Shift_Left(Tmp, Deg(2)-Deg(1));
         Matrix := ( (X,                  One,                Zero),
                     (Tmp xor Carry_Mask, 2**(Deg(2)-Deg(1)),  One) );
      end;

      Deg(2) := Degree(Matrix(2,1), Deg(2));

      if (Deg(2) < Deg(1)) then
         Swap(Matrix);
         Tmp_Deg := Deg(1);
         Deg(1)  := Deg(2);
         Deg(2)  := Tmp_Deg;
      end if;

      while Matrix(1,1) /= One loop
         --
         -- INVARIANT:
         --   Here the following conditions hold
         --
         --     1. Matrix(1,1) /= One
         --     2. Degree(k,1) = Deg(k)
         --     3. Deg(2) >= Deg(1)
         --
         pragma Assert (Matrix(1,1) /= One
                          and Deg(2) >= Deg(1)
                          and Has_Degree(Matrix(1,1), Deg(1))
                          and Has_Degree(Matrix(2,1), Deg(2)),
                        "Invariant does not hold in Long_Inv");

         Combine_Rows (Matrix, Deg(2)-Deg(1));

         Tmp_Deg := Deg(2);
         -- Put_Line("44x X=" & Image(X)
         --            & " 1->" & Image(Matrix(1,1))
         --            & " 2->" & Image(Matrix(2,1)) );
         Deg(2)  := Degree (Matrix(2,1), Deg(2));

         pragma Assert (Tmp_Deg > Deg(2),
                        "Non-decreasing degree");


         if (Deg(2) < Deg(1)) then
            Swap(Matrix);
            Tmp_Deg := Deg(1);
            Deg(1)  := Deg(2);
            Deg(2)  := Tmp_Deg;
         end if;
      end loop;

      pragma Assert (X*Matrix(1,2) = One);

      return Matrix (1, 2);
   end Gf_Long_Inv;

   function GF_Table_Inv (X : Galois) return Galois is
   begin
      return Gf_Exp(-Gf_Log(X));
   end GF_Table_Inv;

   -----------------------------
   -- Multiplication operator --
   -----------------------------

   function "*"(Left, Right: Galois) return Galois is
   begin
      --
      -- If the field is small enough, this function uses the
      -- logarithm table, otherwise it resorts to the "long" product
      -- procedure.
      --

      if (Left = 0 or Right=0) then
         return 0;
      else
         return Product_Callback (Left, Right);
      end if;

      -- if (Log_Table /= null) then
      --    if (Left = 0 or Right=0) then
      --       -- Avoid computing the log of zero
      --       return 0;
      --    else
      --       return Gf_Exp(Gf_Log(Left)+Gf_Log(Right));
      --    end if;
      -- else
      --    return Gf_Long_Product(Left, Right);
      -- end if;
   end "*";

   ----------------------------
   -- Multiplicative inverse --
   ----------------------------

   function Inv(X: Galois) return Galois is
   begin
      if (X = 0) then
         raise Numeric_Error;
      end if;

      return Inverse_Callback(X);

      -- if (Log_Table /= null) then
      --    return Gf_Exp(-Gf_Log(X));
      -- else
      --    return Gf_Long_Inv(X);
      -- end if;
   end Inv;


   -----------------------
   -- Division operator --
   -----------------------

   function "/"(Num, Den: Galois) return Galois is
   begin
      --
      -- If the field is small enough, this function uses the
      -- logarithm table, otherwise it resorts to the "long" product
      -- and inverse procedures.
      --

      -- Check if the division is possible
      if (Den = 0) then
         raise Numeric_Error;
      end if;

      --
      -- If we use the logarithm, we cannot compute the logarithm
      -- of zero. If the logarithm are not used, we resort to the
      -- "long" procedures.  Although Gf_Long_Product checks if the
      -- one of the two operands is zero, checking for Num=0 here
      -- avoids the computation of Inv(Den)
      --
      if (Num=0) then
         return 0;
      else
         return Num*Inv(Den);
      end if;

      -- if (Log_Table /= null) then
      --    return Gf_Exp(Gf_Log(Num)-Gf_Log(Den));
      -- else
      --    return Gf_Long_Product(Num, Gf_Long_Inv(Den));
      -- end if;
   end "/";

   -----------
   -- Image --
   -----------

   function Image(X: Galois) return String is
   begin
      return Interfaces.Unsigned_64'Image(Interfaces.Unsigned_64(X));
   end;

   --
   -- GF(2^n) is a field: the multiplicative inverse of X exists if
   -- and only if X is non null
   --
   function Is_Unit(X: Galois) return Boolean is
   begin
      return X/= 0;
   end Is_Unit;
begin
   -- if (Exponent > Carry_Table'Last) then
   --    raise Numeric_Error;
   -- else
   --    Overflow_Mask := 2**(Exponent-1);
   --    Carry_Mask := Carry_Table(Exponent);
   -- end if;

   if (Exponent > 16 or Small_Footprint) then
      --
      -- Field too big: do not use the logarithm table
      --
      Log_Table := null;
      Exp_Table := null;
      Product_Callback := GF_Long_Product'Access;
      Inverse_Callback := GF_Long_Inv'Access;
   else
      --
      -- GF(2^n) is not too big: create the Log/Exp tables
      --

      -- Log_table e` indicizzata con gli elementi diversi da
      -- zero di GF(2^p)
      Log_Table := new Log_Table_T(1..2**Exponent-1);

      -- Exp_table e` indicizzata con gli interi tra 0 e 2^q-2
      Exp_Table := new Exp_Table_T(0..2**Exponent-2);

  Fill_Table:
      declare
         Generator : Galois := 2;
         Current   : Galois := 1;
      begin
         for Esp in 0..2**Exponent-2 loop
            -- LOOP INVARIANT:
            --      Current = Generator**Esp
            Log_Table(Positive(Current)) := Esp;
            Exp_Table(Esp) := Current;

            -- Sanity check: Current can be equal to one
            -- if and only if Esp=0
            pragma Assert((Current = 1) = (Esp = 0),
                          "Generator^k=1 for k < 2^n-1");

            Current := Gf_Long_Product(Current, Generator);
            -- Here Current = Generator**(Esp+1)
         end loop;

         -- Debug code: to see the exp/log tables change
         -- false with true
         if False then
            for I in Log_Table'Range loop
               Put("Log(");
               Put(Integer'Image(I));
               Put(")=");
               Put(Integer'Image(Log_Table(I)));
               New_Line;
            end loop;

            for I in Exp_Table'Range loop
               Put("Exp(");
               Put(Integer'Image(I));
               Put(")=");
               Put(Integer'Image(Integer(Exp_Table(I))));
               New_Line;
            end loop;
         end if;
      end Fill_Table;

      Product_Callback := GF_Table_Product'Access;
      Inverse_Callback := GF_Table_Inv'Access;
   end if;
end Gf_2p;

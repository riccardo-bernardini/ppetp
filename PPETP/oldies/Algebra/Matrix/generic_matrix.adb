with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Text_Io; use Text_Io;
-- with Algebra;           -- Roba vecchia ?!?
-- with Generic_Utils;     -- Roba vecchia ?!?

package body Generic_Matrix is
   type Operation is (Add, Sub);
   type Scalar_Operation is (Add, Sub, Reversed_Sub, Mul);

   procedure Delete_Buffer is new Ada.Unchecked_Deallocation(Coeff_Matrix,
                                                             Coeff_Buffer);

   -- Write in Result a Bases'length x Ncol Wandermonde matrix
   -- defined as
   --
   --      Result(R, C) := Bases(R)**(C-1)
   --
   -- that is,
   --
   --       1 Bases(1) Bases(1)^2 ... Bases(1)^(Ncol-1)
   --       1 Bases(2) Bases(2)^2 ... Bases(2)^(Ncol-1)
   --       :   :         :                :
   --       :   :         :                :
   --
   procedure Wandermonde(Result: in out Matrix;
                         Ncol:   in     Positive;
                         Bases:  in     Coeff_Array) is
      Acc : Coefficient;
   begin
      Resize_Buffer(X => Result,
                    Nrow => Bases'Length,
                    Ncol => Ncol);

      for Row in Bases'Range loop
         Acc := Coeff_One;

         for Col in 1..Ncol-1 loop
            -- Here Acc = Bases(Row)**(Col-1)
            Result.M(Row, Col) :=  Acc;

            Acc := Acc*Bases(Row);
         end loop;
         -- Here Acc = Bases(Row)**(NCol-1)
         -- (moving this last assignament outside the loop allows me
         -- to save one multiplication per row)
            Result.M(Row, NCol) :=  Acc;
      end loop;
   end Wandermonde;

   procedure Finalize(Object: in out Matrix) is
   begin
      if (Object.M /= null) then
        Delete_Buffer(Object.M);
      end if;
   end Finalize;

   procedure Initialize(Object: in out Matrix) is
   begin
      Object.M := null;
   end Initialize;

   procedure Adjust(Object: in out Matrix) is
      Tmp : Coeff_Buffer;
   begin
      Tmp := Object.M;
      Object.M := new Coeff_Matrix(Tmp.all'Range(1), Tmp.all'Range(2));
      Object.M.all := Tmp.all;
   end Adjust;

   procedure Resize_Buffer(X: in out Matrix; Nrow, Ncol: Positive) is
   begin
      if (X.M = null) then
         X.M := new Coeff_Matrix(1..Nrow, 1..Ncol);
      elsif (X.M.all'Length(1) /= Nrow or
             X.M.all'Length(2) /= Ncol) then
         Delete_Buffer(X.M);
         X.M := new Coeff_Matrix(1..Nrow, 1..Ncol);
      else
         null;
      end if;
   end Resize_Buffer;

   procedure Resize_Buffer(X    : in out Matrix;
                           Nrow : in     Positive;
                           Ncol : in     Positive;
                           Val  : in     Coefficient) is
   begin
      Resize_Buffer(X, Nrow, Ncol);
      X.M.all := (others => (others => Val));
   end Resize_Buffer;

   procedure Resize(X: in out Matrix;
                    Nrow: Positive;
                    Ncol: Positive) is
   begin
      Resize_Buffer(X, NRow, Ncol);
   end Resize;

   procedure Resize(X    : in out Matrix;
                    Nrow : in     Positive) is
   begin
      Resize (X, Nrow, Nrow);
   end Resize;

   procedure Resize_As(X: in out Matrix; Model: Matrix) is
   begin
      Resize_Buffer(X, Model.M'Length(1), Model.M'Length(2));
   end Resize_As;

   procedure Resize_And_Zero(X: in out Matrix;
                             Nrow, Ncol: Positive) is
   begin
      Resize_Buffer(X, NRow, Ncol, Coeff_Zero);
   end Resize_And_Zero;

   function Create(Nrow: Positive;
                   Ncol: Positive;
                   Init: Boolean := True) return Matrix is
      Result: Matrix;
   begin
      Resize(Result, Nrow, Ncol);

      if (Init) then
         Result.M.all := (others => (others => Coeff_Zero));
      end if;

      return Result;
   end Create;

   function Create(Nrow: Positive;
                   Init: Boolean := True) return Matrix is
   begin
      return Create(Nrow, Nrow, Init);
   end Create;

   function Make_Column_Vector(Data: Coeff_Array) return Matrix is
      Result : Matrix;
   begin
      Resize_Buffer(Result, Nrow => Data'Length, Ncol => 1);

      for Row in 1..Size_Row(Result) loop
         Result.M(Row, 1) := Data(Row-1+Data'First);
      end loop;

      return Result;
   end Make_Column_Vector;

   function Make_Row_Vector(Data: Coeff_Array) return Matrix is
      Result : Matrix;
   begin
      Resize_Buffer(Result, Nrow => 1, Ncol => Data'Length);

      for Col in 1..Size_Col(Result) loop
         Result.M(1, Col) := Data(Col-1+Data'First);
      end loop;

      return Result;
   end Make_Row_Vector;

   function Make_Matrix(Data: Coeff_Matrix) return Matrix is
      Result : Matrix;
   begin
      Resize_Buffer(Result,
                    Nrow => Data'Length(1),
                    Ncol => Data'Length(2));

      Result.M.all := Data;
      return Result;
   end Make_Matrix;

   function Zero(Nrow: Positive) return Matrix is
   begin
      return Create(Nrow, Nrow, True);
   end Zero;

   function Zero(Nrow: Positive; Ncol: Positive) return Matrix is
   begin
      return Create(Nrow, Ncol, True);
   end Zero;

   procedure Make_Eye(X: in out Matrix;
                      Nrow: Positive;
                      Ncol : Positive) is
   begin
      Resize_Buffer(X, Nrow, Ncol, Coeff_Zero);

      for I in 1..Positive'Min(Size_Col(X), Size_Row(X)) loop
         X.M(I,I):= Coeff_One;
      end loop;
   end Make_Eye;


   procedure Make_Eje (X    : in out Matrix;
                       Nrow :        Positive) is
   begin
      Resize_Buffer(X, Nrow, Nrow, Coeff_Zero);

      for I in 1..Nrow loop
         X.M(I, Nrow-I+1):= Coeff_One;
      end loop;
   end Make_Eje;



   function Eje(Nrow: Positive) return Matrix is
      Result : Matrix;
   begin
      Make_Eje(Result, Nrow);
      return Result;
   end Eje;

   function One(Nrow: Positive; Ncol: Positive) return Matrix is
      Result : Matrix;
   begin
      Make_Eye(Result, Nrow, Ncol);
      return Result;
   end One;

   function One(Nrow: Positive) return Matrix is
   begin
      return One(Nrow, Nrow);
   end One;

   function Eye(Nrow: Positive; Ncol: Positive) return Matrix is
      Result : Matrix;
   begin
      Make_Eye(Result, Nrow, Ncol);
      return Result;
   end Eye;

   function Eye(Nrow: Positive) return Matrix is
   begin
      return Eye(Nrow, Nrow);
   end Eye;


   function To_Coeff_Matrix(M: Matrix) return Coeff_Matrix is
   begin
      return M.M.all;
   end To_Coeff_Matrix;

   function To_Coeff_Vector(M: Matrix) return Coeff_Array is
      Result : Coeff_Array(1..M.M'Length(1)*M.M'Length(2));
      Row, Col : Positive;
   begin
      for I in Result'Range loop
         Decompose(M, I, Row, Col);
         Result(I) := M.M(Row, Col);
      end loop;

      return Result;
   end To_Coeff_Vector;

   -- function Copy(M: Matrix) return Matrix is
   --    Result : Matrix;
   -- begin
   --    Resize(Result, M.Nrow, M.Ncol);
   --    Result.M.all := M.M.all;
   --
   --    return Result;
   -- end Copy;


   -- function To_idx(M: Matrix; Row: Positive; Col: Natural)
   --                return Positive is
   -- begin
   --    if (Col = 0) then
   --       if (Row > M.Nrow*M.Ncol) then
   --          raise Numeric_Error;
   --       else
   --          return Row;
   --       end if;
   --    else
   --       if (Row > M.Nrow) or (Col > M.Ncol) then
   --          raise Numeric_Error;
   --       else
   --          return Row+(Col-1)*M.Nrow;
   --       end if;
   --    end if;
   -- end To_Idx;


   function Get(M: Matrix; Row, Col: Positive) return Coefficient is
   begin
      return M.M(Row, Col);
   end Get;

   function Get(M: Matrix; Idx: Positive) return Coefficient is
   begin
      if (M.M'Length(1)=1) then
         return M.M(1, Idx);
      elsif (M.M'Length(2)=1) then
         return M.M(Idx, 1);
      else
         declare
            Row, Col: Positive;
         begin
            Decompose(M, Idx, Row, Col);
            return M.M(Row, Col);
         end;
      end if;
   end Get;

   procedure Set(M: Matrix;
                 Row, Col: Positive;
                 Val: Coefficient) is
   begin
      M.M(Row, Col) := Val;
   end Set;

   procedure Set(M: Matrix; Idx: Positive; Val: Coefficient) is
   begin
      if (M.M'Length(1)=1) then
         M.M(1, Idx) := Val;
      elsif (M.M'Length(2)=1) then
         M.M(Idx, 1) := Val;
      else
         declare
            Row, Col: Positive;
         begin
            Decompose(M, Idx, Row, Col);
            M.M(Row, Col) := Val;
         end;
      end if;
   end Set;

   procedure Decompose(M: Matrix;
                       Idx: Positive;
                       Row, Col: out Positive) is
   begin
      -- Devo decomporre Idx in Row e Col in modo da avere
      -- Idx = Row + (Col-1)*Size_row(M)
      if (Idx > Size(M)) then
        raise Constraint_Error;
      end if;

      Row := (Idx-1) mod Size_Row(M) + 1;
      Col := (Idx-Row) / Size_Row(M) + 1;
   end Decompose;


   -- function To_Idx_NC(M: Matrix; Row: Positive; Col: Natural)
   --                 return Positive is
   -- begin
   --    return Row+(Col-1)*M.Nrow;
   -- end To_Idx_NC;
   --
   -- function Get_NC(M: Matrix; Row: Positive; Col: Positive)
   --             return Coefficient is
   -- begin
   --    return M.M(To_Idx_NC(M, Row, Col));
   -- end Get_NC;
   --
   -- procedure Set_NC(M: Matrix; Val: Coefficient; Row: Positive;
   --                                             Col: positive) is
   -- begin
   --    M.m(To_Idx_NC(M, Row, Col)) := Val;
   -- end Set_NC;

   procedure Add_Or_Sub(Result:      in out Matrix;
                        Left, Right: in Matrix;
                        What:        in Operation) is
      Nrow : Positive := Size_Row(Left);
      Ncol : Positive := Size_Col(Left);
   begin
      if (Nrow /= Size_Row(Right)) or (Ncol /= Size_Col(Right)) then
         raise Uncompatible_Sizes;
      end if;

      Resize_Buffer(Result, Nrow, Ncol);
      if (What = Add) then
         for Row in 1..Nrow  loop
            for Col in 1..Ncol  loop
               Result.M(Row, Col) := Left.M(Row, Col) + Right.M(Row, Col);
            end loop;
         end loop;
      else
         for Row in 1..Nrow  loop
            for Col in 1..Ncol  loop
               Result.M(Row, Col) := Left.M(Row, Col) - Right.M(Row, Col);
            end loop;
         end loop;
      end if;
   end Add_Or_Sub;

   procedure Scalar_Op(Result : in out Matrix;
                       Mtx    : in     Matrix;
                       Konst  : in     Coefficient;
                       What   : in     Scalar_Operation) is
   begin
      Resize_As(Result, Mtx);

      case What is
         when Add =>
            for Row in Result.M'Range(1)  loop
               for Col in Result.M'Range(2)  loop
                  Result.M(Row, Col) := Mtx.M(Row, Col) + Konst;
               end loop;
            end loop;
         when Sub =>
            for Row in Result.M'Range(1)  loop
               for Col in Result.M'Range(2)  loop
                  Result.M(Row, Col) := Mtx.M(Row, Col) - Konst;
               end loop;
            end loop;
         when Reversed_Sub =>
            for Row in Result.M'Range(1)  loop
               for Col in Result.M'Range(2)  loop
                  Result.M(Row, Col) := Konst - Mtx.M(Row, Col);
               end loop;
            end loop;
         when Mul =>
            for Row in Result.M'Range(1)  loop
               for Col in Result.M'Range(2)  loop
                  Result.M(Row, Col) := Mtx.M(Row, Col) * Konst;
               end loop;
            end loop;
      end case;
   end Scalar_Op;

   procedure Add(Result:      in out Matrix;
                 Left, Right: in Matrix) is
   begin
      Add_Or_Sub(Result, Left, Right, Add);
   end Add;

   procedure Sub(Result:      in out Matrix;
                 Left, Right: in Matrix) is
   begin
      Add_Or_Sub(Result, Left, Right, Sub);
   end Sub;

   function "+"(E1, E2: Matrix) return Matrix is
      Result : Matrix;
   begin
      Add_Or_Sub(Result, E1, E2, Add);
      return Result;
   end "+";

   function "+"(E1: Matrix; K : Coefficient) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Add);
      return Result;
   end "+";

   function "+"(K : Coefficient; E1 : Matrix) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Add);
      return Result;
   end "+";


   function "-"(E1, E2: Matrix) return Matrix is
      Result : Matrix;
   begin
      Add_Or_Sub(Result, E1, E2, Sub);
      return Result;
   end "-";

   function "-"(E1: Matrix; K : Coefficient) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Sub);
      return Result;
   end "-";

   function "-"(K : Coefficient; E1 : Matrix) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Reversed_Sub);
      return Result;
   end "-";

   procedure Negate(X: in out Matrix) is
   begin
      for Row in 1..Size_Row(X)  loop
         for Col in 1..Size_Col(X)  loop
            X.M(Row, Col) := -X.M(Row, Col);
         end loop;
      end loop;
   end Negate;

   function "-"(E: Matrix) return Matrix is
      Result : Matrix;
   begin
      Result := E;
      Negate(Result);
      return Result;
   end "-";

   function "*"(E1, E2: Matrix) return Matrix is
      Result : Matrix;
      Acc : Coefficient;
   begin
      if (Size_Col(E1) /= Size_Row(E2)) then
         raise Uncompatible_Sizes;
      end if;

      Resize_Buffer(Result, Size_Row(E1), Size_Col(E2));
      for Row in 1..Size_Row(Result) loop
         for Col in 1..Size_Col(Result) loop
            Acc := Coeff_Zero;
            for Inner in 1..Size_Col(E1) loop
               Acc := Acc + E1.M(Row, Inner)*E2.M(Inner, Col);
            end loop;

            Result.M(Row, Col) := Acc;
         end loop;
      end loop;

      return Result;
   end "*";

   function "*"(E1: Matrix; K : Coefficient) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Mul);
      return Result;
   end "*";

   function "*"(K : Coefficient; E1 : Matrix) return Matrix is
      Result : Matrix;
   begin
      Scalar_Op(Result, E1, K, Mul);
      return Result;
   end "*";


   function "="(X, Y: Matrix) return Boolean is
   begin
      if (Is_Empty(X) and Is_Empty(Y)) then
         return True;
      end if;

      if (Size_Row(X) /= Size_Row(Y) or Size_Col(X) /= Size_Col(Y)) then
         return False;
      end if;

      for Row in 1..Size_Row(X) loop
         for Col in 1..Size_Col(Y) loop
            if (X.M(Row,Col) /= Y.M(Row,Col)) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end "=";

   function Size_Row(X: Matrix) return Natural is
   begin
      if (X.M = null) then
         return 0;
      else
         return X.M.all'Length(1);
      end if;
   end Size_Row;

   function Size_Col(X: Matrix) return Natural is
   begin
      if (X.M = null) then
         return 0;
      else
         return X.M.all'Length(2);
      end if;
   end Size_Col;

   function Size(X: Matrix) return Natural is
   begin
      return Size_Row(X)*Size_Col(X);
   end Size;


   procedure Swap_Columns(Mtx: in out Matrix;
                          Col1, Col2: Positive) is
      Tmp : Coefficient;
   begin
      for Row in Mtx.M.all'Range(1) loop
         Tmp := Get(Mtx, Row, Col1);
         Mtx.M(Row, Col1) := Mtx.M(Row, Col2);
         Mtx.M(Row, Col2) := Tmp;
      end loop;
   end Swap_Columns;

   procedure Multiply_Column(Mtx: in out Matrix;
                             Col: in Positive;
                             Const: in Coefficient) is
   begin
      -- Put_Line("MC in");
      -- for Row in Mtx.M'Range(1) loop
      --    for col in Mtx.M'Range(2) loop
      --       Put(Image(Mtx.M(Row, Col)) & "  ");
      --    end loop;
      --    Put_Line("");
      -- end loop;
      -- Put_Line("%%%%");

      for Row in Mtx.M'Range(1) loop
         -- Put_line("(r,c)=("
         --          & Integer'Image(Row)
         --          & ", "
         --          & Integer'Image(Col) & ")");
         Mtx.M(Row, Col) := Mtx.M(Row, Col)*Const;
      end loop;
      --Put_Line("MC out");
   end Multiply_Column;

   procedure Sum_Columns(Mtx: in out Matrix;
                         From, To: in Positive;
                         Const: in Coefficient) is
   begin
      for Row in Mtx.M.all'Range loop
         Mtx.M(Row, To) := Mtx.M(Row, To) + Const*Mtx.M(Row, From);
      end loop;
   end Sum_Columns;

   procedure Inv_And_Det(E: in Matrix;
                         Inverse: out Matrix;
                         Det: out Coefficient) is
      E_Copy : Matrix;
      Nrow : Positive := Size_Row(E);
      Ncol : Positive := Size_Col(E);
      Idx : Natural;
      Alpha : Coefficient;
   begin
      if (Is_Empty(E) or Nrow /= Ncol) then
        raise Not_Square;
      end if;

      E_Copy  := E;
      Inverse := Eye(Nrow);
      Det     := Coeff_One;

      for Row in 1..Nrow loop

         -- Search for a non-null element in row Row
         Idx := 0;
         for Col in Row..Ncol loop
            if (E_Copy.M(Row, Col) /= Coeff_Zero) then
              Idx := Col;
              exit;
            end if;
         end loop;

         if (Idx = 0) then
            Det := Coeff_Zero;
            return;
         end if;

         -- Swap the column Idx (the one with the non-null element)
         -- with column Row in order to have the Row-th diagonal element
         -- different from zero
         Swap_Columns(E_Copy, Row, Idx);
         Swap_Columns(Inverse, Row, Idx);

         -- Update the determinant value
         Det   := Det * E_Copy.M(Row, Row);

         -- Now divide the Row-th column by the value of the diagonal
         -- element in order to have the  Row-th diagonal element equal
         -- to One
         Alpha := Inv(E_Copy.M(Row, Row));
         Multiply_Column(E_Copy,  Col => Row, Const => Alpha);
         Multiply_Column(Inverse, Col => Row, Const => Alpha);

         -- Now add the Row-th column to the other ones in order
         -- to put to zero every element on the Row-th row, but
         -- the diagonal one
         for Col in 1..Ncol loop
            if (Col /= Row) then
               Alpha := -E_Copy.M(Row, Col);
               Sum_Columns(E_Copy,  From => Row, To => Col, Const => Alpha);
               Sum_Columns(Inverse, From => Row, To => Col, Const => Alpha);
            end if;
        end loop;
      end loop;
   end Inv_And_Det;

   function Inv(E: Matrix) return Matrix is
      D : Coefficient;
      Result : Matrix;
   begin
      Inv_And_Det(E, Det => D, Inverse => Result);
      if (D = Coeff_Zero) then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end Inv;

   function Determinant(E: Matrix) return Coefficient is
      Result : Coefficient;
      Junk : Matrix;
   begin
      Inv_And_Det(E, Det => Result, Inverse => Junk);
      return Result;
   end Determinant;

   function Is_Unit(E: Matrix) return Boolean is
   begin
      return Is_Unit(Determinant(E));
   end Is_Unit;

   function Is_Empty(E: Matrix) return Boolean is
   begin
      return E.M = null;
   end Is_Empty;

   procedure Check_Row_Index(M: Matrix; Row: Pos_Array) is
   begin
      for I in Row'Range loop
         if (Row(I) > M.M'Length(1)) then
            raise Constraint_Error;
         end if;
      end loop;
   end Check_Row_Index;

   procedure Check_Col_Index(M: Matrix; Col: Pos_Array) is
   begin
      for I in Col'Range loop
         if (Col(I) > M.M'Length(2)) then
            raise Constraint_Error;
         end if;
      end loop;
   end Check_Col_Index;

   procedure Check_Indexes(M: Matrix; Row, Col: Pos_Array) is
   begin
      Check_Row_Index(M, Row);
      Check_Col_Index(M, Col);
   end Check_Indexes;

   procedure Check_Indexes(M: Matrix; Row: Positive; Col: Pos_Array) is
   begin
      if (Row > M.M'Last(1)) then
         raise Constraint_Error;
      end if;

      Check_Col_Index(M, Col);
   end Check_Indexes;

   procedure Check_Indexes(M: Matrix; Row: Pos_Array; Col: Positive) is
   begin
      Check_Row_Index(M, Row);
      if (Col > M.M'Last(2)) then
         raise Constraint_Error;
      end if;
   end Check_Indexes;

   -- procedure Stampa(S: String; X: Pos_Array) is
   -- begin
   --    Put (Integer'Image(X'First) & ";" & Integer'Image(X'Last) & " ");
   --    Put(S & "=(");
   --    for I in X'Range loop
   --       Put(Integer'Image(X(I)));
   --       Put(",");
   --    end loop;
   --
   --    Put_Line(")");
   -- end Stampa;

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Pos_Array;
                            Col : in     Pos_Array;
                            Src : in     Matrix) is
   begin
      Check_Indexes(Dst, Row, Col);
      if (Row'Length /= Src.M'Length(1) or
          Col'Length /= Src.M'Length(2)) then
         raise Constraint_Error;
      end if;

      for R in Row'Range loop
         for C in Col'Range loop
            Dst.M(Row(R), Col(C)) := Src.M(R,C);
         end loop;
      end loop;
   end Put_Submatrix;


   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Positive;
                            Col : in     Pos_Array;
                            Src : in     Matrix) is
   begin
      Check_Indexes(Dst, Row, Col);
      if (Src.M'Length(1) /= 1 or
          Src.M'Length(2) /= Col'Length) then
         raise Constraint_Error;
      end if;

      for C in Col'Range loop
         Dst.M(Row, Col(C)) := Src.M(1,C);
      end loop;
   end Put_Submatrix;

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Pos_Array;
                            Col : in     Positive;
                            Src : in     Matrix) is
   begin
      Check_Indexes(Dst, Row, Col);
      if (Src.M'Length(1) /= Row'Length or
          Src.M'Length(2) /= 1) then
         raise Constraint_Error;
      end if;

      for R in Row'Range loop
         Dst.M(Row(R), Col) := Src.M(R,1);
      end loop;
   end Put_Submatrix;


   function Get_Submatrix(M: Matrix;
                          Row, Col: Pos_Array) return Matrix is
      Result : Matrix;
   begin
      Check_Indexes(M, Row, Col);

      Resize_Buffer(Result, Row'Length, Col'Length);
      for R in Row'Range loop
         for C in Col'Range loop
            Result.M(R,C) := M.M(Row(R), Col(C));
         end loop;
      end loop;

      return Result;
   end Get_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row: Pos_Array;
                          Col: Positive) return Matrix is
      Result : Matrix;
   begin
      Check_Indexes(M, Row, Col);

      Resize_Buffer(Result, Row'Length, 1);
      for R in Row'Range loop
         Result.M(R,1) := M.M(Row(R), Col);
      end loop;

      return Result;
   end Get_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row, Col: Positive) return Matrix is
      Result : Matrix;
   begin
      Resize_Buffer(Result, 1, 1);
      Result.M(1,1) := M.M(Row, Col);

      return Result;
   end Get_Submatrix;


   function Get_Submatrix(M: Matrix;
                          Row: Positive;
                          Col: Pos_Array) return Matrix is
      Result : Matrix;
   begin
      Check_Indexes(M, Row, Col);

      Resize_Buffer(Result, 1, Col'Length);
      for C in Col'Range loop
         Result.M(1,C) := M.M(Row, Col(C));
      end loop;

      return Result;
   end Get_Submatrix;

   function Transpose(M: Matrix) return Matrix is
      Result : Matrix;
   begin
      Resize_Buffer(Result, M.M'Length(2), M.M'Length(1));

      for Row in Result.M'Range(1) loop
         for Col in Result.M'Range(2) loop
            Result.M(Row, Col) := M.M(Col, Row);
         end loop;
      end loop;

      return Result;
   end Transpose;

   function Reshape(M: Matrix;
                    New_Row, New_Col: Positive) return Matrix is
   begin
      if (M.M'Length(1)*M.M'Length(2) /= New_Col*New_Row) then
         raise Constraint_Error;
      end if;

      declare
         Result : Matrix;
         Row_Src, Col_Src : Positive;
         Row_Dst, Col_Dst : Positive;
      begin
         Resize_Buffer(Result, New_Row, New_Col);

         for I in 1..New_Row*New_Col loop
            Decompose(M, I, Row_Src, Col_Src);
            Decompose(Result, I, Row_Dst, Col_Dst);
            Result.M(Row_Dst, Col_Dst) := M.M(Row_Src, Col_Src);
         end loop;
         return Result;
      end;
   end Reshape;

   function Range_To_Array(R : Index_Ranges.Actual_Range) return Pos_Array is
      N_Steps : Integer;
   begin
      -- So start, stop e step. Cerco il minimo n tale che
      --
      --   n*step + start > stop
      --
      -- ho che deve essere n > (stop-start)/step da cui
      -- n = floor(1+(stop-start)/step)
      N_Steps := Integer(Float'Floor(1.0+Float(R.Stop-R.Start)/Float(R.Step)));
      declare
         Result : Pos_Array(1..N_Steps);
      begin
         for I in Result'Range loop
            Result(I) := R.Start + (I-1)*R.Step;
         end loop;

         return Result;
      end;
   end Range_To_Array;

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Index_Ranges.Index_Range;
                            Col : in     Index_Ranges.Index_Range;
                            Src : in     Matrix) is
      Row_Range, Col_Range : Index_Ranges.Actual_Range;
   begin
      Row_Range := Index_Ranges.Get_Extremes(Row, Dst.M'Last(1));
      Col_Range := Index_Ranges.Get_Extremes(Col, Dst.M'Last(2));

      Put_Submatrix(Dst,
                    Range_To_Array(Row_Range),
                    Range_To_Array(Col_Range),
                    Src);
   end Put_Submatrix;

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Positive;
                            Col : in     Index_Ranges.Index_Range;
                            Src : in     Matrix) is
      Col_Range : Index_Ranges.Actual_Range;
   begin
      Col_Range := Index_Ranges.Get_Extremes(Col, Dst.M'Last(2));

      Put_Submatrix(Dst,
                    Row,
                    Range_To_Array(Col_Range),
                    Src);
   end Put_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row: Index_Ranges.Index_Range;
                          Col: Index_Ranges.Index_Range) return Matrix is
      Row_Range, Col_Range : Index_Ranges.Actual_Range;
   begin
      Row_Range := Index_Ranges.Get_Extremes(Row, M.M'Last(1));
      Col_Range := Index_Ranges.Get_Extremes(Col, M.M'Last(2));

      return Get_Submatrix(M,
                           Range_To_Array(Row_Range),
                           Range_To_Array(Col_Range));
   end Get_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row: Positive;
                          Col: Index_Ranges.Index_Range) return Matrix is
      Col_Range : Index_Ranges.Actual_Range;
   begin
      Col_Range := Index_Ranges.Get_Extremes(Col, M.M'Last(2));

      return Get_Submatrix(M, Row, Range_To_Array(Col_Range));
   end Get_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row: Index_Ranges.Index_Range;
                          Col: Positive) return Matrix is
      Row_Range : Index_Ranges.Actual_Range;
   begin
      Row_Range := Index_Ranges.Get_Extremes(Row, M.M'Last(2));

      return Get_Submatrix(M, Range_To_Array(Row_Range), Col);
   end Get_Submatrix;

   function Get_Submatrix(M: Matrix;
                          Row: Pos_Array;
                          Col: Index_Ranges.Index_Range) return Matrix is
      Col_Range : Index_Ranges.Actual_Range;
   begin
      Col_Range := Index_Ranges.Get_Extremes(Col, M.M'Last(2));

      return Get_Submatrix(M, Row, Range_To_Array(Col_Range));
   end Get_Submatrix;


   function Get_Submatrix(M: Matrix;
                          Row: Index_Ranges.Index_Range;
                          Col: Pos_Array) return Matrix is
      Row_Range : Index_Ranges.Actual_Range;
   begin
      Row_Range := Index_Ranges.Get_Extremes(Row, M.M'Last(2));

      return Get_Submatrix(M, Range_To_Array(Row_Range), Col);
   end Get_Submatrix;

   function H_Concat(Left, Right: Matrix) return Matrix is
     Result : Matrix;
   begin
      if (Size_Row(Left) /= Size_Row(Right)) then
         raise Uncompatible_Sizes;
      end if;

      Resize(Result, Size_Row(Left), Size_Col(Left)+Size_Col(Right));

      for Col in Left.M'Range(2)  loop
        for Row in Result.M'Range(1) loop
           Result.M(Row, Col) := Left.M(Row, Col);
        end loop;
      end loop;

      for Col in Right.M'Range(2) loop
        for Row in Result.M'Range(1) loop
           Result.M(Row, Left.M'Last(2)+Col) := Right.M(Row, Col);
        end loop;
      end loop;

      return Result;
   end H_Concat;

   function V_Concat(Over, Under: Matrix) return Matrix   is
      Result : Matrix;
   begin
      if (Size_Col(Over) /= Size_Col(Under)) then
         raise Uncompatible_Sizes;
      end if;

      Resize(Result, Size_Row(Over)+Size_Row(Under), Size_Col(Over));

      for Row in Over.M'Range(1)  loop
        for Col in Result.M'Range(2) loop
           Result.M(Row, Col) := Over.M(Row, Col);
        end loop;
      end loop;

      for Row in Over.M'Range(1)  loop
        for Col in Result.M'Range(2) loop
           Result.M(Over.M'Last(1)+Row, Col) := Under.M(Row, Col);
        end loop;
      end loop;
      return Result;
   end V_Concat;

   function Diag(X: Coeff_Array) return Matrix  is
      Result : Matrix;
   begin
      Resize_And_Zero(Result, X'Length, X'Length);

      for I in X'Range loop
         Result.M(I,I) := X(I);
      end loop;

      return Result;
   end Diag;

   function Diag(X: Matrix) return Matrix  is
      Result : Matrix;
   begin
      if (X.M'Length(1) > 1 and X.M'Length(2) > 1) then
         -- Here X is a matrix => extract its diagonal

         declare
            New_Len : Positive;
         begin
            if (X.M'Length(1) > X.M'Length(2)) then
               New_Len := X.M'Length(2);
            else
               New_Len := X.M'Length(1);
            end if;

            Resize(Result, New_Len, 1);
            for I in Result.M'Range(1) loop
               Result.M(I,1) := X.M(I,I);
            end loop;
         end;
      elsif (X.M'Length(1) > 1) then
         -- Here x.m'length(1)>1 and x.m'length(2)=1, i.e.,
         -- X is column vector.
         --
         -- Create a diagonal matrix

         Resize_And_Zero(Result, X.M'Length(1), X.M'Length(1));

         for I in Result.M'Range(1) loop
            Result.M(I,I) := X.M(I,1);
         end loop;
      elsif (X.M'Length(2) > 1) then
         -- Here x.m'length(1)=1 and x.m'length(2)>1, i.e.,
         -- X is a row vector => Create a diagonal matrix

         Resize_And_Zero(Result, X.M'Length(2), X.M'Length(2));

         for I in Result.M'Range(1) loop
            Result.M(I,I) := X.M(1,I);
         end loop;
      else
         -- Here x.m is 1x1.  The result of diag is equal to X
         Result := X;
      end if;

      return Result;
   end Diag;

   function FlipLR(X: Matrix) return Matrix  is
      Result : Matrix;
   begin
      Resize_As(Result, X);

      for Row in Result.M'Range(1) loop
         for Col in Result.M'Range(2) loop
            Result.M(Row, Col) := X.M(Row, (1+X.M'Last(2))-Col);
         end loop;
      end loop;

      return Result;
   end FlipLR;

   function FlipUD(X: Matrix) return Matrix is
     Result : Matrix;
   begin
      Resize_As(Result, X);

      for Row in Result.M'Range(1) loop
         for Col in Result.M'Range(2) loop
            Result.M(Row, Col) := X.M((1+X.M'Last(1))-Row, Col);
         end loop;
      end loop;

      return Result;
   end FlipUD;

   function Map(X: Matrix; Fun: Elementary_Function) return Matrix is
      Result : Matrix;
   begin
      Resize_As(Result, X);

      for Row in Result.M'Range(1) loop
         for Col in Result.M'Range(2) loop
            Result.M(Row, Col) := Fun(X.M(Row,Col));
         end loop;
      end loop;

      return Result;
   end Map;

   procedure Map_In_Place(X: in out Matrix; Fun: Elementary_Function) is
   begin
      for Row in X.M'Range(1) loop
         for Col in X.M'Range(2) loop
            X.M(Row, Col) := Fun(X.M(Row,Col));
         end loop;
      end loop;
   end Map_In_Place;


end Generic_Matrix;

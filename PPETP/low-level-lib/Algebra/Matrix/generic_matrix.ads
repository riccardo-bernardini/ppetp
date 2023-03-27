--                              -*- Mode: Ada -*-
--  Filename        : generic_matrix.ads
--  Description     : Generic package for matrices in a ring
--  Author          : Finta Tartaruga
--  Created On      : Mon Mar 10 23:01:13 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!
--  Type            : <GENERIC>

--
-- This package provides few functions for working with matrices with
-- elements in a commutative ring.
--

with Ada.Finalization; use Ada.Finalization;
with Index_Ranges;

generic
   -- Type of matrix elements
   type Coefficient is private;

   -- 1 and 2-dimensional arrays of matrix elements
   type Coeff_Array  is
     array (positive range <>) of Coefficient;

   type Coeff_Matrix is
     array (positive range <>, positive range <>) of Coefficient;

   -- "One" and "Zero" (i.e. neutral elements of product and sum)
   -- of the ring of matrix elements
   Coeff_Zero: Coefficient;
   Coeff_One:  Coefficient;

   -- Ring operations
   with function "+"(E1, E2: Coefficient) return Coefficient is <>;
   with function "-"(E1, E2: Coefficient) return Coefficient is <>;
   with function "-"(E: Coefficient)      return Coefficient is <>;
   with function "*"(E1, E2: Coefficient) return Coefficient is <>;

   -- Ring inverse. Called only if Is_Unit(E) is true
   with function Inv(E: Coefficient) return Coefficient is <>;

   -- Return true if E is a unit of the ring, i.e., if E has
   -- a multiplicative inverse.
   with function Is_unit(E: Coefficient) return Boolean is <>;
   -- with function Image(E: Coefficient) return String;

package Generic_Matrix is
   Uncompatible_sizes : exception;
   Not_square : exception;

   type Matrix is private;
   type Pos_Array is array (positive range <>) of Positive;


   type Elementary_Function is
     access function(X: Coefficient) return Coefficient;

   --
   -- Function to create new matrices
   --
   function Create (Nrow : Positive;
                    Ncol : positive;
                    Init : Boolean := true)
                  return Matrix;

   function Create (Nrow : Positive;
                    Init : Boolean := true)
                  return Matrix;

   procedure Resize (X    : in out Matrix;
                     Nrow : in     Positive;
                     Ncol : in     Positive);

   procedure Resize (X    : in out Matrix;
                     Nrow : in     Positive);


   procedure Resize_As (X     : in out Matrix;
                        Model : in     Matrix);

   --
   -- Transform 1-dim and 2-dim arrays into a Matrix
   --
   function Make_Column_Vector(Data: Coeff_Array) return Matrix;
   function Make_Row_Vector(Data: Coeff_Array)    return Matrix;
   function Make_Matrix(Data: Coeff_Matrix)       return Matrix;

   -- Neutral element of sum, i.e., null matrix
   function Zero(Nrow: Positive) return Matrix; -- square matrix
   function Zero(Nrow: Positive; Ncol: Positive) return Matrix;

   -- Identity matrix
   function One(Nrow: Positive) return Matrix; -- square matrix
   function One(Nrow: Positive; Ncol: Positive) return Matrix;

   function Eye(Nrow: Positive) return Matrix; -- square matrix
   function Eye(Nrow: Positive; Ncol: Positive) return Matrix;

   -- Reverse Identity matrix
   function Eje(Nrow: Positive) return Matrix; -- square matrix

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
   procedure Wandermonde (Result: in out Matrix;
                          Ncol:   in     Positive;
                          Bases:  in     Coeff_Array);
   --
   -- Matrix operation, operator syntax
   --
   function "+" (E1, E2: Matrix) return Matrix;
   function "+" (E1: Matrix; K : Coefficient) return Matrix;
   function "+" (K : Coefficient; E1: Matrix) return Matrix;

   function "-" (E1, E2: Matrix) return Matrix;
   function "-" (K : Coefficient; E1 : Matrix) return Matrix;
   function "-" (E1 : Matrix; K : Coefficient) return Matrix;

   function "*" (E1, E2: Matrix) return Matrix;
   function "*" (K : Coefficient; E1 : Matrix) return Matrix;
   function "*" (E1 : Matrix; K : Coefficient) return Matrix;

   function "-" (E: Matrix) return Matrix;

   --
   -- Matrix operation, assembler-like syntax
   --
   procedure Add (Result : in out Matrix; Left, Right : in Matrix);
   procedure Sub (Result : in out Matrix; Left, Right : in Matrix);
   procedure Negate (X : in out Matrix);

   --
   -- Determinant and inverse
   --
   procedure Inv_And_Det (E       : in     Matrix;
                          Inverse :    out Matrix;
                          Det     :    out Coefficient);


   function Inv (E : Matrix) return Matrix;
   function Determinant (E: Matrix) return Coefficient;
   function Is_Unit (E: Matrix) return boolean;

   --
   -- Return the matrix obtained by applying Fun to every element
   -- of X
   --
   function Map (X: Matrix; Fun: Elementary_Function) return Matrix;

   --
   -- Same as "Map", but operate "in place" (i.e., write the result
   -- back on X)
   --
   procedure Map_In_place (X: in out Matrix; Fun: Elementary_Function);

   --
   -- Boolean functions
   --
   function Is_Empty (E: Matrix) return Boolean;
   function "=" (X, Y: Matrix) return Boolean;

   --
   -- Function to access matrix elements
   --
   function Get (M: Matrix; Row, Col: Positive) return Coefficient;
   procedure Set (M: Matrix; Row, col: Positive; Val: Coefficient);

   -- Column-wise access "a la" FORTRAN (or Matlab)
   function Get (M: Matrix; Idx: Positive) return Coefficient;
   procedure Set (M: Matrix; Idx: Positive; Val: Coefficient);


   -- Extract a submatrix
   function Get_Submatrix (M   : Matrix;
                           Row : Positive;
                           Col : Positive) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Pos_Array;
                           Col : Pos_array) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Pos_Array;
                           Col : positive) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Positive;
                           Col : Pos_Array) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Index_Ranges.Index_Range;
                           Col : Index_Ranges.Index_Range) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Positive;
                           Col : Index_Ranges.Index_Range) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Index_Ranges.Index_Range;
                           Col : Positive) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Pos_Array;
                           Col : Index_Ranges.Index_Range) return Matrix;

   function Get_Submatrix (M   : Matrix;
                           Row : Index_Ranges.Index_Range;
                           Col : Pos_Array) return Matrix;

   -- Write a submatrix
   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Pos_Array;
                            Col : in     Pos_Array;
                            Src : in     Matrix);

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Pos_Array;
                            Col : in     Positive;
                            Src : in     Matrix);


   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Positive;
                            Col : in     Pos_array;
                            Src : in     Matrix);


   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Index_Ranges.Index_Range;
                            Col : in     Index_Ranges.Index_Range;
                            Src : in     Matrix);

   procedure Put_Submatrix (Dst : in out Matrix;
                            Row : in     Positive;
                            Col : in     Index_Ranges.Index_Range;
                            Src : in     Matrix);


   ---
   --- "Matlab-like" functions
   ---
   function H_Concat (Left, Right : Matrix) return Matrix;
   function V_Concat (Over, Under : Matrix) return Matrix;
   function Diag (X : Coeff_Array) return Matrix;
   function Diag (X : Matrix) return Matrix;

   function FlipLR (X : Matrix) return Matrix;
   function FlipUD (X : Matrix) return Matrix;

   function Transpose(M : Matrix) return Matrix;
   function Reshape(M : Matrix;
                    New_Row, New_Col : Positive) return Matrix;
   ---
   ---
   ---

   function To_Coeff_Matrix(M : Matrix) return Coeff_Matrix;
   function To_Coeff_Vector(M : Matrix) return Coeff_Array;

   --
   -- Matrix dimensions
   --
   function Size_Row(X : Matrix) return Natural;
   function Size_Col(X : Matrix) return Natural;
   function Size(X : Matrix)     return Natural;
private
   type Coeff_Buffer is access Coeff_Matrix;

   type Matrix is new Controlled with record
      -- Nrow, Ncol: Natural;
      M: Coeff_Buffer;
   end record;


   procedure Finalize(Object: in out Matrix);
   procedure Initialize(Object: in out Matrix);
   procedure Adjust(Object: in out Matrix);

   procedure Decompose(M: Matrix; Idx: Positive; Row, Col: out Positive);

   procedure Resize_Buffer(X: in out Matrix; Nrow, Ncol: Positive);
end Generic_Matrix;

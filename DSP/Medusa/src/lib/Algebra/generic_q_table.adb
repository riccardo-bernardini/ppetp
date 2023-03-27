with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Q_Table is
   package Numeric_Coeff_Pkg is
      new Ada.Numerics.Generic_Elementary_Functions (Coefficient_Type);

   use Numeric_Coeff_Pkg;

   -----------------
   -- Best_Q_Step --
   -----------------

   procedure Best_Q_Step (Table    : in     Q_Table_Type;
                          Required : in     Coefficient_Type;
                          Obtained :    out Coefficient_Type;
                          Index    :    out Integer) is
      Top    : Integer;
      Bottom : Integer;
      Half   : Integer;
   begin
      pragma Assert (Table(Bottom) < Table(Top));

      -- Since the table is in increasing order, we will use a bisection
      -- approach to find the N such that Table(N) >= Required >= Table(N+1).
      -- The value to be returned will be chosen between Table(N) and
      -- Table(N+1).

      Top    := Table'First;  -- The two extremes used in the
      Bottom := Table'Last;   -- bisection procedure.

      -- We need to handle two easy cases first; we need to check that
      -- the table has at least two values (I know, it sounds silly...)
      -- and that the required value is between the table bounds.
      if (Top = Bottom) then
         -- The table has only one entry: not much choice
         Index := Bottom;
         Obtained := Table(Bottom);
         return;
      end if;

      -- Here Top > Bottom

      if (Required <= Table(Bottom)) then
         -- The required value is not larger than the smallest
         -- quantization step: use the smallest step.
         Index := Bottom;
         Obtained := Table(Bottom);
         return;
      end if;

      if (Required >= Table(Top)) then
         -- The required value is not smaller than the largest
         -- quantization step: use the largest step.

         Index    := Top;
         Obtained := Table(Top);
         return;
      end if;

      -- Now the "difficult" case.  If I am here, the required value
      -- is STRICTY larger than Table(Bottom) and STRICTY SMALLER
      -- than Table(Top).

      while (Top > Bottom+1) loop
         -- Here Table(Bottom) < Required < Table(Top) and
         -- Top >= Bottom+2
         pragma Assert (Table(Bottom) <  Required   and
                             Required <  Table(Top) and
                                  Top >= Bottom+2);

         Half := (Top+Bottom)/2;
         -- Since Top >= bottom+2 we have
         --
         --           half >= bottom+1
         --           half <= top-1.
         --
         -- It follows that half /= bottom and half /= top.  Therefore,
         -- the difference Top-Bottom at the next iteration will
         -- be smaller than the same difference at this iteration.  It
         -- follows that the while loop will terminate.

         if (Required = Table(Half) then
            Index := Half;
            Obtained := Table(Half);
         elsif (Required > Table(Half)) then
            Bottom := Half;
         else
            Top := Half;
         end if;
      end loop;
      pragma Assert(Top = Bottom+1);


      if (Required-Table(Bottom) > Table(Top)-Required) then
         Index := Top;
      else
         Index := Bottom;
      end if;

      Obtained := Table(Index);
   end Best_Q_Step;

   ------------
   -- Q_Step --
   ------------

   function Q_Step (Table : Q_Table_Type;
                    Idx   : Integer)
                   return Coefficient_Type   is
   begin
      return Table(Idx);
   end Q_Step;

   ------------
   -- Resize --
   ------------

   procedure Resize (Table : in out Q_Table_Type;
                     Size  : in     Positive)
   is
   begin
      Table := new Coefficient_Array(0..Size-1);
   end Resize;

   ----------------
   -- Set_Q_Step --
   ----------------

   procedure Set_Q_Step (Table : in out Q_Table_Type;
                         Idx   : in     Integer;
                         Value : in     Coefficient_Type)
   is
   begin
      Table(Idx) := Value;
   end Set_Q_Step;

   ----------------
   -- Fill_Table --
   ----------------

   procedure Fill_Table (Table    : in out Q_Table_Type;
                         N_Levels : in     Positive;
                         Q_Max    : in     Coefficient_Type;
                         Q_Min    : in     Coefficient_Type)
   is
      Step  : constant Coefficient_Type :=
        (Q_Max/Q_Min)**(1.0/Coefficient_Type(N_Levels));
   begin
      Resize(Table, N_Levels);
      for I in Table'Range loop
         Table(I) := Q_Min * Step**(I-Table'First);
      end loop;
   end Fill_Table;

end Generic_Q_Table;

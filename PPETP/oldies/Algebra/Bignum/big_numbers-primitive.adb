with Ada.Text_IO;                 use Ada.Text_IO;
package body big_numbers.primitive is
   procedure Dump (X : Bigint_Holder;
                   S : String := "") is
   begin
      Put_Line ("[" & S & "]");
      for I in X'Range loop
         Put_Line (Integer'Image (I) & " -> "
                   & Bigint_Entry'Image (X (I)));
      end loop;
   end Dump;
   subtype Carry_Type is Bigint_Entry range 0 .. 1;

   ---------------
   -- Extend_In --
   ---------------

   --
   -- Copy Value of Src into Holder Dst, padding the unused
   -- entry with zeros.
   --
   procedure Extend_In (Src : in     Big_Int;
                        Dst :    out Bigint_Holder) is
   begin
      pragma Assert (Src.Value'Length <= Dst'Length);

      Dst (Src.Value'First .. Src.Value'Last) := Src.Value.all;
      Dst (Src.Value'Last+1 .. Dst'Last) := (others => 0);
   end Extend_In;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Value  : in     Positive;
                        buffer :    out Bigint_Holder) is
      Tmp : Natural;
   begin
      Tmp := Value;
      for I in Buffer'Range loop
         Buffer (I) := Bigint_Entry(Tmp mod Natural(Base));
         Tmp := Tmp / Natural(Base);
      end loop;
   end Set_Value;

   ---------
   -- Add --
   ---------

   procedure Add (Accumulator : in out Bigint_Holder;
                  Operand     : in     Bigint_Holder) is
      Carry : Carry_Type;
      Tmp   : Bigint_Entry;
   begin
      pragma Assert (Accumulator'Length = Operand'Length);
      Carry := 0;
      for I in Accumulator'Range loop
         pragma Assert (Accumulator (I) < Base);
         pragma Assert (Operand (I) < Base);
         Tmp := Accumulator (I) + Operand (I);
         Accumulator (I) := Tmp mod Base;
         Carry  := Tmp / Base;
      end loop;
   end Add;

   ---------
   -- Sub --
   ---------

   procedure Sub (Accumulator : in out Bigint_Holder;
                  Operand     : in     Bigint_Holder) is
      procedure Do_Borrow (X     : in out Bigint_Holder;
                           Start : in     Positive) is
         Borrow_From : Natural := Start - 1;
      begin
         for J in Start .. X'Last loop
            pragma Assert (not (J = X'Last and X(J) /= 0));
            if X (J) = 0 then
               X (J) := Base - 1;
            else
               X(J) := X(J) - 1;
               return;
            end if;
         end loop;

         raise Program_Error; -- We should never arrive here
      end Do_Borrow;
   begin
      for I in Accumulator'Range loop
         if (Accumulator (I) > Operand (I)) then
            Accumulator (I) := Accumulator (I) - Operand (I);
         else
            Accumulator (I) := Accumulator (I) + (Base - Operand (I));
            Do_Borrow (Accumulator, I+1);
         end if;
      end loop;
   end Sub;


   ----------
   -- Mult --
   ----------

   procedure Mult (Vector : in     Bigint_Holder;
                   Scalar : in     Bigint_Entry;
                   Result :    out Bigint_Holder) is
      Carry : Bigint_Entry;
      Tmp   : Bigint_Entry;
   begin
      Carry := 0;
      for I in Vector'Range loop
         Tmp := Vector (I) * Scalar + Carry;
         Result (I) := Tmp mod Base;
         Carry := Tmp / Base;
      end loop;

      pragma Assert (Carry = 0);
   end Mult;

   -----------
   -- Shift --
   -----------

   procedure Shift (Value  : in out Bigint_Holder;
                    Amount : in     Natural) is
   begin
      if (Amount = 0) then
         return;
      elsif (Amount >= Value'Length) then
         Value := (others => 0);
      else
         Value (Amount .. Value'Last) := Value (0 .. Value'Last - Amount);
         Value (0 .. Amount - 1)      := (others => 0);
      end if;
   end Shift;



   ----------
   -- Mult --
   ----------

   procedure Mult (Result :     out Bigint_Holder;
                   Long   : in      Bigint_Holder;
                   Short  : in      Bigint_Holder) is
      Buffer : Bigint_Holder (Result'Range);
      Buf_Op : Bigint_Holder := Long;
   begin
      Result := (others => 0);
      for I in Short'Range loop
         Mult (Result => Buffer,
               Vector => Buf_Op,
               Scalar => Short (I));

         Add (Accumulator => Result, Operand => Buffer);
         Shift (Buf_Op, 1);
      end loop;
   end Mult;

   function Get_Top (X : Bigint_Holder) return Natural is
   begin
      for I in reverse X'First .. X'Last loop
         if (X (I) /= 0) then
            return I;
         end if;
      end loop;

      return 0;
   end Get_Top;

   ---------
   -- Cmp --
   ---------

   function Cmp (Left, Right : Bigint_Holder) return sign_type is
      Top_Left  : Natural := Get_Top (Left);
      Top_Right : Natural := Get_Top (Right);
   begin
      if (Top_Left < Top_Right) then
         return -1;
      elsif (Top_Left > Top_Right) then
         return 1;
      else
         for I in reverse 0..Top_Left loop
            if (Left(I) > Right(I)) then
               return 1;
            elsif (Left(I) < Right(I)) then
               return -1;
            end if;
         end loop;

         return 0;
      end if;
   end Cmp;

   function Is_Zero (X : Bigint_Holder) return Boolean is
   begin
      for I in X'Range loop
         if (X (I) /= 0) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Zero;

   ----------
   -- Half --
   ----------

   procedure Half (X : in out Bigint_Holder) is
      Carry : Carry_Type;
      New_Carry : Carry_Type;
   begin
      Carry := 0;
      for I in reverse X'Range loop
         New_Carry := X(I) mod 2;
         X(I) := X(I)/2;
         if (Carry = 1) then
            X(I) := X(I) + Base/2;
         end if;

         Carry := New_Carry;
      end loop;
   end Half;


   ------------
   -- Divmod --
   ------------

   procedure Divmod (Num       : in     Bigint_Holder;
                     Den       : in     Bigint_Holder;
                     Quotient  :    out Bigint_Holder;
                     Remainder :    out Bigint_Holder) is
      -------------
      -- Average --
      -------------

      procedure Average (Result  :    out Bigint_Holder;
                         Left    : in     Bigint_Holder;
                         Right   : in     Bigint_Holder) is
      begin
         Result := Left;
         Add (Accumulator => Result, Operand => Right);
         Half(Result);
      end Average;

      N_Shift  : Natural;
      Top      : Bigint_Holder (0..Num'Last + 1);
      Bottom   : Bigint_Holder (0 .. Num'Last + 1);
      Middle   : Bigint_Holder (0 .. Num'Last + 1);
      C_Top    : Bigint_Holder (0..Num'Last - Den'Last + 1);
      C_Bottom : Bigint_Holder (0 .. Num'Last - Den'Last + 1);
      C_Middle : Bigint_Holder (0 .. Num'Last - Den'Last + 1);
   begin
      Top   := (others => 0);
      Top (Den'Range) := Den;
      C_Top := (0 => 1, others => 0);
      N_Shift := 0;

      -- Here:
      --  Top = Den * C_Top
      --  C_Top = Base^N_Shift

      Dump (Top, "top");
      Dump (Num, "num");
      Put_Line (Sign_Type'Image(Cmp (Top, Num)));

      while Cmp(Top, Num) < 0 loop
         C_Bottom := C_Top;
         Bottom := Top;
         Shift(Top, 1);
         Shift(C_Top, 1);
         N_Shift := N_Shift + 1;

         -- Here:
         --  Top      = Den * C_Top
         --  C_Top    = Base^N_Shift
         --  Bottom   = Den * C_Bottom
         --  C_Bottom = Base^(N_Shift-1)
      end loop;
      pragma Assert (N_Shift > 0);
      -- Here
      --   Top > Num > Bottom
      --   Top = Den*C_Top
      --   Bottom = Den*C_Bottom
      --   C_Top    = Base^N_Shift
      --   Delta = C_Top-C_Bottom = Base ** (N_Shift-1)*(Base-1)

      Add(C_Top, C_Bottom);
      Add(Top,    Bottom);

      -- Here
      --     C_Top-C_Bottom = Base ** N_Shift = 2 ** (B*N_Shift)

      for I in 1 .. (N_Shift * Log_2_Base)loop
         -- Here
         --   Top  > Num > Bottom
         --   Top    = C_Top    * Den
         --   Bottom = C_Bottom * Den
         --   Middle = C_Middle * Den
         --
         --   Top - Middle
         --      = Middle - Bottom
         --      = 2 ** (B*N_Shift - (I-1))

         Average(Middle,   Top,   Bottom);
         Average(C_Middle, C_Top, C_Bottom);

         -- Here
         --   Top    = C_Top    * Den
         --   Bottom = C_Bottom * Den
         --   Middle = C_Middle * Den
         --
         --   Top - Middle
         --      = Middle - Bottom
         --      = 2 ** (B*N_Shift - I)

         case Cmp(Middle, Num) is
            when 1 =>
               Top   := Middle;
               C_Top := C_Middle;
            when 0 =>
               Bottom   := Middle;
               C_Bottom := C_Middle;
               exit;
            when -1 =>
               Bottom   := Middle;
               C_Bottom := Middle;
         end case;
      end loop;

      -- Here
      --   C_Top - C_Middle = 1

      Quotient  := C_Bottom;
      Remainder := Num;
      Sub (Remainder, Bottom);
   end Divmod;
end big_numbers.primitive;

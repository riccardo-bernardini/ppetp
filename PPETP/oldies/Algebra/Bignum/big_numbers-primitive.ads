private package  Big_Numbers.Primitive is
   procedure Extend_In (Src : in     Big_Int;
                        Dst :    out Bigint_Holder);

   procedure Set_Value (Value  : in     Positive;
                        buffer :    out Bigint_Holder);


   procedure Add (Accumulator : in out Bigint_Holder;
                  Operand     : in     Bigint_Holder);

   procedure Sub (Accumulator : in out Bigint_Holder;
                  Operand     : in     Bigint_Holder);


   procedure Mult (Vector : in     Bigint_Holder;
                   Scalar : in     Bigint_Entry;
                   Result :    out Bigint_Holder);

   procedure Shift (Value  : in out Bigint_Holder;
                    Amount : in     Natural);

   procedure Mult (Result :     out Bigint_Holder;
                   Long   : in      Bigint_Holder;
                   Short  : in      Bigint_Holder);

   procedure Divmod (Num       : in     Bigint_Holder;
                     Den       : in     Bigint_Holder;
                     Quotient  :    out Bigint_Holder;
                     Remainder :    out Bigint_Holder);

   function Get_Top (X : Bigint_Holder) return Natural;
   function Cmp (Left, Right : Bigint_Holder) return Sign_Type;
   function Is_Zero (X : Bigint_Holder) return Boolean;

   procedure Dump (X : Bigint_Holder;
                   S : String := "");
end Big_Numbers.Primitive;

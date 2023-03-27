package body Index_Ranges is
   function Idx_Range(From : Positive;
                      To   : Positive;
                      step : integer := 1) return Index_Range is
   begin
      return Idx_Range(To_Range_Extreme(From),
                       To_Range_Extreme(to),
                       step);
   end Idx_Range;

   function Idx_Range(From : Positive;
                      To   : Range_Extreme;
                      step : integer := 1) return Index_Range is
   begin
      return Idx_Range(To_Range_Extreme(From),
                       To,
                       step);
   end Idx_Range;


   function Idx_Range(From : Range_Extreme;
                      To   : Positive;
                      step : integer := 1) return Index_Range is
   begin
      return Idx_Range(From,
                       To_Range_Extreme(to),
                       Step);
   end Idx_Range;


   function Idx_Range(From : Range_Extreme;
                      To   : Range_extreme;
                      step : integer := 1) return Index_Range is
      Result : Index_Range := (Start => From, Stop => To, Step => Step);
   begin
      return Result;
   end Idx_Range;

   function Full_Range return Index_Range is
   begin
      return Idx_Range(To_Range_Extreme(1), Last, 1);
   end Full_Range;

   function Last return Range_Extreme is
     Result : Range_Extreme;
   begin
      Result.R := new Range_Extreme_Core'(What => Last_Entry);

      return Result;
   end Last;


   function To_Range_Extreme(N: Integer) return Range_Extreme is
      Result : Range_Extreme;
   begin
      Result.R := new Range_Extreme_Core' (What => Const, Value => N);
      return Result;
   end To_Range_Extreme;

   function "+"(L, R: Range_Extreme) return Range_Extreme is
      Result : Range_Extreme;
   begin
      Result.r := new Range_Extreme_Core'(What => add, Left => L.R, Right => R.R);
      return Result;
   end "+";

   function "-"(L, R: Range_Extreme) return Range_Extreme is
      Result : Range_Extreme;
   begin
      result.R := new Range_Extreme_Core'(What => sub, Left => L.R, Right => R.R);
      return Result;
   end "-";

   function "*"(L, R: Range_Extreme) return Range_Extreme is
      result : Range_Extreme;
   begin
      result.R := new Range_Extreme_Core'(What => Mul, Left => L.R, Right => R.R);
      return Result;
   end "*";

   function "/"(L, R: Range_Extreme) return Range_Extreme is
      result : Range_Extreme;
   begin
      result.R := new Range_Extreme_Core'(What => Div, Left => L.R, Right => R.R);
      return Result;
   end "/";

   function "+"(R: Range_Extreme; N: Integer) return Range_Extreme is
   begin
      return R + To_Range_Extreme(N);
   end "+";

   function "+"(N: Integer; R: Range_Extreme) return Range_Extreme is
   begin
      return To_Range_Extreme(N) + R;
   end "+";

   function "-"(R: Range_Extreme; N: Integer) return Range_Extreme is
   begin
      return R - To_Range_Extreme(N);
   end "-";

   function "-"(N: Integer; R: Range_Extreme) return Range_Extreme is
   begin
      return To_Range_Extreme(N) - R;
   end "-";

   function "*"(R: Range_Extreme; N: Integer) return Range_Extreme is
   begin
      return R * To_Range_Extreme(N);
   end "*";

   function "*"(N: Integer; R: Range_Extreme) return Range_Extreme is
   begin
      return To_Range_Extreme(N) * R;
   end "*";

   function "/"(R: Range_Extreme; N: Integer) return Range_Extreme is
   begin
      return R / To_Range_Extreme(N);
   end "/";

   function "/"(N: Integer; R: Range_Extreme) return Range_Extreme is
   begin
      return To_Range_Extreme(N) / R;
   end "/";

   function Eval(R: Range_Extreme_pt; Last_Index: Positive) return Integer is
   begin
      case R.What is
         when Const =>
           return R.Value;
         when Last_Entry =>
            return Last_Index;
         when Add =>
            return Eval(R.left, Last_Index)+Eval(r.right, Last_Index);
         when sub =>
            return Eval(R.left, Last_Index)-Eval(r.right, Last_Index);
         when mul =>
            return Eval(R.left, Last_Index)*Eval(r.right, Last_Index);
         when div =>
            return Eval(R.left, Last_Index)/Eval(r.right, Last_Index);
      end case;
   end Eval;

   function Eval(R: Range_Extreme; Last_Index: Positive) return Integer is
   begin
      return Eval(R.R, Last_Index);
   end Eval;

   function Get_Extremes(I    : in     Index_Range;
                         Lst  : in     Positive) return Actual_Range is
      Result : Actual_Range;
   begin
      result.start := Eval(I.Start, Lst);
      result.stop  := Eval(I.Stop,  Lst);
      result.Step := I.Step;

      return Result;
   end Get_Extremes;
end Index_Ranges;

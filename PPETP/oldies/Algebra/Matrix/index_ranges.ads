package Index_Ranges is
   type Index_Range is private;
   type Actual_Range is record 
      Start, Stop, step : Integer;
   end record;
   
   type Range_Extreme is private;
   
   function Idx_Range(From : Positive; 
		      To   : Positive;
		      step : integer := 1) return Index_Range; 
   
   function Idx_Range(From : Positive; 
		      To   : Range_Extreme; 
		      step : integer := 1) return Index_Range; 
   
   function Idx_Range(From : Range_Extreme;
		      To   : Positive; 
		      step : integer := 1) return Index_Range; 
   
   function Idx_Range(From : Range_Extreme;
		      To   : Range_extreme; 
		      step : integer := 1) return Index_Range; 
   
   function Full_Range return Index_Range;
   
   function Last return Range_Extreme;
   
   function To_Range_Extreme(N: Integer) return Range_Extreme;
   
   function "+"(R: Range_Extreme; N: Integer) return Range_Extreme;
   function "-"(R: Range_Extreme; N: Integer) return Range_Extreme;
   function "*"(R: Range_Extreme; N: Integer) return Range_Extreme;
   function "/"(R: Range_Extreme; N: Integer) return Range_Extreme;
   
   function "+"(N: Integer; R: Range_Extreme) return Range_Extreme;
   function "-"(N: Integer; R: Range_Extreme) return Range_Extreme;
   function "*"(N: Integer; R: Range_Extreme) return Range_Extreme;
   function "/"(N: Integer; R: Range_Extreme) return Range_Extreme;
   
   function "+"(L, R: Range_Extreme) return Range_Extreme;
   function "-"(L, R: Range_Extreme) return Range_Extreme;
   function "*"(L, R: Range_Extreme) return Range_Extreme;
   function "/"(L, R: Range_Extreme) return Range_Extreme;
   
   function Eval(R: Range_Extreme; Last_Index: Positive) return Integer;
   
   function Get_Extremes(I    : in     Index_Range;
			 Lst  : in     Positive) return Actual_Range;
private
   type Extreme_Type is (Const, Last_Entry, Add, Sub, Mul, Div);
   
   type Index_Range is record
      Start, Stop : Range_Extreme;
      Step : Integer;
   end record;
   
   type Range_Extreme_Core;
   type Range_Extreme_Pt is access Range_Extreme_Core;
   type Range_Extreme_Core(What : Extreme_Type) is record      
      case What is
	 when Const =>
	   Value : Integer;
	 when Last_Entry =>
	    null;
	 when Add | Sub | Mul | Div =>
	    Left, Right : Range_Extreme_Pt;
      end case;
   end record;
   
   type Range_Extreme is record
      R : Range_Extreme_Pt;
   end record;
end Index_Ranges;

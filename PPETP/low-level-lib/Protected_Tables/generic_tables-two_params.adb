package body Generic_Tables.Two_Params is

   Temp_Aux  : Second_Type;
   Temp_Again: Third_Type;
   Temp_Proc : Callback_Two;

   procedure Internal_Callback (X : Element) is
   begin
      Temp_Proc(X, Temp_Aux, Temp_Again);
   end Internal_Callback;

   procedure Iterate (T    : in out Table;
                      Proc :        Callback_Two;
                      Aux  :        Second_Type;
                      Again:        Third_Type) is
   begin
      Temp_Aux  := Aux;
      Temp_Again:= Again;
      Temp_Proc := Proc;
      T.Iterate(Internal_Callback'Access);
   end Iterate;
end Generic_Tables.Two_Params;

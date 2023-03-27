generic
   type Second_Type is private;
   type Third_Type is private;
package Generic_Tables.Two_Params is
   type Callback_Two is
     access procedure (X : Element; Y : Second_Type; Z : Third_Type);

   procedure Iterate (T    : in out Table;
                      Proc :        Callback_Two;
                      Aux  :        Second_Type;
                      Again:        Third_Type);
end Generic_Tables.Two_Params;

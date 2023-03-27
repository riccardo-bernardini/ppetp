package body Timestamps is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Crumb_Timestamp) return Boolean is
   begin
      if (Left.Net_Time = Right.Net_Time) then
         return Left.Portion < Right.Portion;
      else
         return Left.Net_Time < Right.Net_Time;
      end if;
   end "<";
end Timestamps;


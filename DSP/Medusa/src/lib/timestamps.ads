package Timestamps is
   type Multimedia_Timestamp is mod 2**32;


   -- type Crumb_Timestamp is record
   --    Net_Time : Multimedia_Timestamp;
   --    Portion  : Positive;
   -- end record;
   --
   -- function "<"(Left, Right : Crumb_Timestamp) return Boolean;
end Timestamps;

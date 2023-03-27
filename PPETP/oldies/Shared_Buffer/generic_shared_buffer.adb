package body Generic_Shared_Buffer is

   -------------------
   -- Index_Handler --
   -------------------

   protected body Index_Handler is

      -------------------
      -- Begin_Writing --
      -------------------

      entry Begin_Writing (Where :    out Natural)
      when  N_Ready < Size is
      begin
         pragma Assert(not Writing, "Begin_Writing called twice");
         Where   := First_Free;
         Writing := True;
      end Begin_Writing;

      ------------------
      -- Done_Writing --
      ------------------

      procedure Done_Writing (Where : in     Natural) is
      begin
         pragma Assert(Writing, "Done_Writing without Begin");
         pragma Assert(Where = First_Free, "Done_Writing with wrong index");
         N_Ready    := N_Ready + 1;
         First_Free := (First_Free + 1) mod Size;
         Writing    := False;
      end Done_Writing;

      --------------------
      -- Cancel_Writing --
      --------------------

      procedure Cancel_Writing (Where : in     Natural) is
      begin
         pragma Assert(Writing, "Cancel_Writing without Begin");
         pragma Assert(Where = First_Free, "Cancel_Writing with wrong index");
         Writing    := False;
      end Done_Writing;

      -------------------
      -- Begin_Reading --
      -------------------

      entry Begin_Reading (Where :    out Natural)
      when N_Ready > 0 is
      begin
         pragma Assert(not Reading, "Begin_Reading called twice");
         Where   := (First_Free - N_Ready) mod Size;
         Reading := True;
      end Begin_Reading;

      ------------------
      -- Done_Reading --
      ------------------

      procedure Done_Reading (Where : in     Natural) is
      begin
         pragma Assert(Reading, "Done_Reading without Begin");
         pragma Assert(Where = (First_Free - N_Ready) mod Size,
                         "Done_Reading with wrong index");
         N_Ready := N_Ready - 1;
         Reading := False;
      end Done_Reading;

      --------------------
      -- Cancel_Reading --
      --------------------

      procedure Cancel_Reading (Where : in     Natural) is
      begin
         pragma Assert(Reading, "Done_Reading without Begin");
         pragma Assert(Where = (First_Free - N_Ready) mod Size,
                         "Done_Reading with wrong index");
         Reading := False;
      end Done_Reading;

      ---------------
      -- Any_Ready --
      ---------------

      function Any_Ready return Boolean is
      begin
         return N_Ready > 0;
      end Any_Ready;

      ----------
      -- Full --
      ----------

      function Full return Boolean is
      begin
         return N_Ready = Size;
      end Full;

      function How_Many_Ready   return Natural is
      begin
         return N_Ready;
      end How_Many_Ready;

      function How_Many_Free    return Natural is
      begin
         return Size - N_Ready;
      end How_Many_Free;

      function How_Many_Entries return Natural is
      begin
         return Size;
      end How_Many_Entries;


   end Index_Handler;

end Generic_Shared_Buffer;

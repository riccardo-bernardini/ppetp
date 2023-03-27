package body Ignore_Packet_List is
   Whole_Packet : constant Natural := 0;

   -----------------
   -- Ignore_List --
   -----------------

   protected body Basic_Ignore_List is

      ------------
      -- Insert --
      ------------

      procedure Insert (Timestamp : in Multimedia_Timestamp) is
      begin
         Timestamp_Lists.Insert(My_List, Timestamp);
      end Insert;

      --------------
      -- Contains --
      --------------

      function Contains
        (Timestamp : in Multimedia_Timestamp)
         return Boolean
      is
      begin
         return Timestamp_Lists.Contains (My_List, Timestamp);
      end Contains;

   end Basic_Ignore_List;

   ----------
   -- Init --
   ----------
   procedure Init (List         : in out Ignore_List;
                   N_Components : in     Positive) is
   begin
      if (Ignore_List.Internal_Lists /= null) then
         raise Program_Error;
      end if;

      Ignore_List.Internal_Lists := new Basic_List_Array(0..N_Components);
   end Init;

   -------------------
   -- Ignore_Packet --
   -------------------

   procedure Ignore_Packet  (List      : in out Ignore_List;
                             Timestamp : in     Multimedia_Timestamp;
                             Component : in     Component_Index)
   is
   begin
      Internal_Lists(Component).Insert(Timestamp);
   end Ignore_Packet;

   procedure Ignore_Packet (List      : in out Ignore_List;
                            Timestamp : in     Multimedia_Timestamp)
   is
   begin
      for Portion in Internal_Lists'Range loop
         Internal_Lists(Portion).Insert(Timestamp);
      end loop;
   end Ignore_Packet;

   ----------------------
   -- Is_To_Be_Ignored --
   ----------------------

   function Is_To_Be_Ignored (Timestamp : Multimedia_Timestamp;
                              Component : Component_Index)
                             return Boolean is
   begin
      return Internal_Lists(Component).Contains(Timestamp);
   end;

   function Is_To_Be_Ignored (Timestamp : Multimedia_Timestamp)
                             return Boolean is
   begin
      -- First check if timestamp is contained in the
      -- "whole packet" list
      if (Internal_Lists(Whole_Packet).Contains(Timestamp)) then
         return True;
      end if;

      -- If I am here Timestamp is not in the whole packet list.
      -- This does not imply that Timestamp is not to be ignored.
      -- Actually, it could happen that two-argument Ignore_Packet
      -- was called for every value of portion. Therefore, we
      -- need to check all the portions.
      for Portion in 1..Internal_Lists'Last loop
         if (not Internal_Lists(Portion).Contains(Timestamp)) then
            return False;
         end if;
      end loop;

      -- All the portions are to be ignored: return true, but
      -- first register Timestamp in the whole_packet list.
      Internal_Lists(Whole_Packet).Insert(Timestamp);
      return True;
   end Is_To_Be_Ignored;

end Ignore_Packet_List;

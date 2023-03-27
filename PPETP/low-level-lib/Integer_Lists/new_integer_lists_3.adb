with Ada.Unchecked_Deallocation;
with Ada.Text_IO; 		use ada.Text_IO;

package body New_Integer_Lists_3 is

   Object_Count : Natural := 0;

   procedure Delete is
      new Ada.Unchecked_Deallocation(Item_Buffer, Item_Buffer_ref);



   -------------------
   -- Set_Sup_Limit --
   -------------------
   procedure Set_Sup_Limit(List  : in out New_Integer_List_3;
                           Limit : in Item_Type) is
   begin
      List.Sup_Limit := Limit;
   end Set_Sup_Limit;


   ------------------
   -- In_Win_Range --
   ------------------
   function In_Win_Range(List : in New_Integer_List_3;
                         Item : in Item_Type) return Boolean is
   begin

      -- 0 <= First - L <= First <= Sup <= Last
      if (List.First >= List.Buffer'Length) and (Item_Type'Last - List.First >= List.Sup_Limit) then
         --Put_Line("In_Win_Range 1");
         declare
            Inf : Item_Type := List.First-List.Buffer'Length +1;
            Sup : Item_Type := List.First+List.Sup_Limit;
         begin
            if Item in Inf..Sup then
               return True;
            else
               return False;
            end if;
         end;
      end if;

      -- First -L <= 0 <= First <= Sup
      if List.First < List.Buffer'Length then
        -- Put_Line("In_Win_Range 2");
         declare
            Sup : Item_Type := List.First + List.Sup_Limit;
            Inf : Item_Type := List.First - List.Buffer'Length+1;
         begin
            if Item in 0..Sup then
               return True;
            elsif Item in Inf..Item_Type'Last then
               return True;
            else
               return False;
            end if;
         end;
      end if;

      -- First-L <= First <= Last <= Sup
      if List.Sup_Limit > Item_Type'Last - List.First then
        -- Put_Line("In_Win_Range 3");
         declare
            Inf : Item_Type := List.First-List.Buffer'Length +1;
            Sup : Item_Type := List.First + List.Sup_Limit;
         begin
            if Item in Inf .. Item_Type'Last then
               return True;
            elsif Item in 0.. Sup then
               return True;
            else
               return False;
            end if;
         end;
      end if;

      --Put_Line("In_Win_Range 4");
      -- Out of the window
      return False;

   end  In_Win_Range;

   ---------------------
   -- In_Buffer_Range --
   ---------------------
   function In_Buffer_Range(List : in New_Integer_List_3;
                            Item : in Item_Type) return Boolean is
   begin

      if List.First >= List.Buffer'Length-1 then
         if Item  in List.First-List.Buffer'Length+1 .. List.First then
            return True;
         else
            return False;
         end if;
      else
         if (Item in 0..List.First) or (Item in  List.First-List.Buffer'Length+1..Item_Type'Last) then
            return True;
         else
            return False;
         end if;
      end if;

   end In_Buffer_Range;


   -------------------
   -- In_High_Range --
   -------------------
   function In_High_Range(List : in New_Integer_List_3;
                          Item : in Item_Type) return Boolean is
   begin

      -- First <  Sup <= Last
      if Item_Type'Last - List.First >= List.Sup_Limit then
         if Item in List.First+1 .. List.First+List.Sup_Limit then
            return True;
         else
            return False;
         end if;

      else  -- First <= Last < Sup
         if (Item in List.First+1..Item_Type'Last) or (Item in 0..List.First+List.Sup_Limit) then
            return True;
         else
            return False;
         end if;

      end if;

   end In_High_Range;


   --------------------------
   -- Item_To_Buffer_Index --
   --------------------------
   function Item_To_Buffer_Index (List : in  New_Integer_List_3;
                                  Item : in  Item_Type) return Item_Type is
   begin
      return ((Item mod List.Buffer'Length) + List.Offset) mod List.Buffer'Length;
   end Item_To_Buffer_Index;


   ------------
   -- Insert --
   ------------
   procedure Insert (List         : in out New_Integer_List_3;
                     Item         : in     Item_Type;
                     On_Duplicate : in     Duplicate_Policy := Die) is
   begin

      -- Is the first item insert in the list
      if List.Empty then
         List.First := Item;
         -- The first element is stored in the last position of the buffer
         List.Offset := (List.Buffer'Length-1) - (Item mod List.Buffer'Length);
         List.Buffer( List.Buffer'Length -1 ) := True;

         List.Empty := False;
         return;
      end if;

--        if In_Win_Range(List,Item) then
--           Put_Line(Item'img &" Is in Win");
--        end if;


      -- Out of the Window
      if not In_Win_Range(List,Item) then
         case On_Duplicate is
            when Die =>
               raise Program_Error;
            when Ignore =>
               return;
         end case;
      end if;



      -- In the buffer range
--        if List.First >= List.Buffer'Length-1 then
--           if Item  in List.First-List.Buffer'Length+1 .. List.First then
--              -- Yet in the buffer
--           --   Put_Line("Insert : In buffer range");
--              if List.Buffer( Item_To_Buffer_Index(List, Item)) then
--                 case On_Duplicate is
--                 when Die =>
--                    raise Program_Error;
--                 when Ignore =>
--                    return;
--                 end case;
--              end if;
--
--              List.Buffer( Item_To_Buffer_Index(List, Item)) := True;
--              return;
--           end if;
--        else
--           if (Item in 0..List.First) or (Item in  List.First-List.Buffer'Length+1..Item_Type'Last) then
--              -- Yet in the buffer
--       --       Put_Line("Insert : In buffer range");
--              if List.Buffer( Item_To_Buffer_Index(List, Item)) then
--                 case On_Duplicate is
--                 when Die =>
--                    raise Program_Error;
--                 when Ignore =>
--                    return;
--                 end case;
--              end if;
--
--              List.Buffer( Item_To_Buffer_Index(List, Item)) := True;
--              return;
--           end if;
--
--        end if;

      -- In the buffer range
      if In_Buffer_Range(List, Item) then
           -- Yet in the buffer
--           --   Put_Line("Insert : In buffer range");
            if List.Buffer( Item_To_Buffer_Index(List, Item)) then
               case On_Duplicate is
               when Die =>
                  raise Program_Error;
               when Ignore =>
                  return;
               end case;
            end if;

            List.Buffer( Item_To_Buffer_Index(List, Item)) := True;
            return;
      end if;


      -- In the Sup Limit
      if List.First < Item then
         for i in Item_Type range List.First+1 .. Item loop
            List.Buffer( Item_To_Buffer_Index(List, i) ) := False;
         end loop;
      else
         for i in Item_Type range List.First+1 .. Item_Type'Last loop
            List.Buffer( Item_To_Buffer_Index(List, i) ) := False;
         end loop;

         for i in Item_Type range 0..Item loop
            List.Buffer( Item_To_Buffer_Index(List, i) ) := False;
         end loop;

      end if;

      List.Buffer( Item_To_Buffer_Index(List, Item)) := True;
      List.First := Item;



   end Insert;


   --------------
   -- Contains --
   --------------
   function Contains (List : in New_Integer_List_3;
                      Item : in Item_Type) return Boolean is
   begin

      if In_Buffer_Range(List, Item) then
         return List.Buffer( Item_To_Buffer_Index(List, Item));
      elsif In_High_Range(List, Item) then
         return False;
      else
         return True;
      end if;
   end Contains;


   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(Object: in out New_Integer_List_3)
   is
   begin

      Object_Count := Object_Count + 1;		-- Package variable

      Object.Buffer := new Item_Buffer'(0..Table_Size-1 => False);
      Object.First  := Item_Type'First;
      Object.Offset := 0;
     -- Object.Inf_Limit := 0;
      Object.Sup_Limit := 0;
      Object.Empty := True;
      Object.Entity_ID := Object_Count;
   end Initialize;


   ----------
   -- Dump --
   ----------
   procedure Dump(List : in New_Integer_List_3) is
--      Inf : Item_Type := List.First - List.Inf_Limit;
      Sup : Item_Type := List.First + List.Sup_Limit;
   begin
      Put_Line("First   = " & List.First'img);
      Put_Line("Offset  = " & List.Offset'img);
      Put_Line("Sup_Lim = " & Sup'img);
  --    Put_Line("Inf_Lim = " & Inf'img);


      if List.First >= List.Buffer'Length-1 then
         for i in Item_Type range List.First - List.Buffer'Length +1 .. List.First loop
            Put_Line ( i'img & " -> " & List.Buffer(Item_To_Buffer_Index(List, i))'img);
         end loop;
      else

         for i in Item_Type range  List.First - List.Buffer'Length +1.. Item_Type'Last loop
            Put_Line ( i'img & " -> " & List.Buffer(Item_To_Buffer_Index(List, i))'img);
         end loop;

         for i in Item_Type range 0 .. List.First loop
            Put_Line ( i'img & " -> " & List.Buffer(Item_To_Buffer_Index(List, i))'img);
         end loop;
      end if;
   end;

   ---------
   -- "=" --
   ---------
   function "=" (Left, Right : New_Integer_List_3_pt) return Boolean is
   begin
      return Left.Entity_ID = Right.Entity_ID;
   end "=";


end New_Integer_Lists_3;


with Text_Io;
use  Text_Io;

with Ada.Unchecked_Deallocation;

package body Integer_Lists is
   --
   -- A Integer_List is implemented by means of a dinamically allocated
   -- vector of booleans.  In order to avoid an indefinite growth of
   -- the vector, we keep a value First and we adopt the convention
   -- that every timestamp less than First is contained in the list,
   -- every timestamp greater or equal than Buffer'Length+First
   -- is not in the list and the timestamp >= First and <
   -- Buffer'Length+First are in the list if and only if
   -- Buffer(Timestamp mod Buffer'Length) is true.
   --
   -- We can picture this approach as follows: think about a
   -- buffer of infinite length which is viewed through a window of
   -- length equal to the buffer length. The window is updated
   -- in order to have the n-th value in the (n mod Length)-th
   -- entry of the vector.
   --

   --
   -- More formally, let f : N -> {T, F} be a function. and suppose
   -- that f(n) = F for all but a finite number of n.  Let First \in N
   -- be such that f(n)=T for every n<First and f(First)=F.  Suppose
   -- that Len is such that  f(n)=F for every n > First+Len.
   -- Within the above hypothesis we can describe
   -- f by using Buf : array(0..Len-1) of Boolean as follows
   --
   --    f(n) = T  if n < First
   --           F  if n = First or n > First+Len
   --           Buf(n mod Len) if First+1 <= n <= First+Len
   --
   -- Note that with this approach f(n) is always mapped to the same
   -- entry of Buf => no data movement is required.
   --
   -- We will call Border = First+1 mod Len the location used by
   -- the first stored value of f(n)
   --
   -- In the above description we supposed Len known and fixed.  In a real
   -- case this is not necessarily true and it could be necessary to
   -- increase the buffer size.  We will choose of taking always a buffer
   -- length equal to a power of two.  This grants that the new length
   -- New_Len will satisfy New_len >= 2*Len.
   --
   -- Note that increasing the buffer size does not change First,
   -- but it can change the value of Border since First mod Len
   -- can be different from First mod New_Len
   --
   procedure Delete is
      new Ada.Unchecked_Deallocation(Item_Buffer, Item_Buffer_ref);


   function Next_Pow_2(X: Item_Type) return Item_Type is
      Result : Item_Type := 1;
   begin
      if (X > Item_Type'Last/2) then
         raise Storage_Error;
      end if;

      while (Result < X) loop
         Result := Result*2;
      end loop;

      return Result;
   end Next_Pow_2;

   --======--
   -- DUMP --
   --======--
   --
   -- Print on the stdout a string of '0' and '1' representing
   -- the content of buffer X.  Useful for debugging purposes
   --
   procedure Dump(X : Item_Buffer) is
   begin
      for I in X'Range loop
         if (I mod 8 = 0) then
            Put ("|");
         end if;

         if (X(I)) then
            Put("1");
         else
            Put("0");
         end if;
      end loop;
      New_Line;
   end Dump;

   --===============--
   -- RESIZE_BUFFER --
   --===============--
   --
   -- Enlarge the buffer of List in order to make possible
   -- to store Item in it.
   --
   procedure Resize_Buffer(List : in out Integer_List;
                           Item : in     Item_Type) is
      -- Save the reference to the current buffer
      Old_Buffer   : Item_Buffer(List.Buffer'Range);
      Old_border   : Item_Type;
      New_border   : Item_Type;
      New_Len      : Item_Type;
      Old_Len      : Item_Type;
      Last_Written : Item_Type;
   begin
      -- It's simpler if we first copy the whole old buffer
      -- in a new one without the wrap-around effect.  Maybe it
      -- not the most efficient approach, but we expect
      -- that this function will not be called too often
      Old_Len := List.Buffer'Length;
      Old_Border := List.First mod Old_Len;

      -- Put_Line ("-->" & Item_Type'Image(Old_Border));
      -- Put_Line (Item_Type'image(List.Buffer'Last));
      -- Put_Line (Item_Type'image(List.Buffer'First));

      if (Old_Border /= 0) then
         Old_Buffer := List.Buffer(Old_Border..Old_Buffer'Last) &
           List.Buffer(0..Old_Border-1);
      else
         Old_Buffer := List.Buffer(Old_Border..Old_Buffer'Last);
      end if;

      -- Now we can delete the old buffer...
      Delete(List.Buffer);

      -- ...and allocate a new one.  First compute the new length.
      -- We need a buffer whose length is at least Item-First+1,
      -- but it must be also a power of two
      New_Len := Next_Pow_2(Item-List.First+1);

      -- Allocate and init the new buffer
      List.Buffer := new Item_Buffer'(0..New_Len-1 => False);
      -- List.Buffer.all := (others => False);

      -- Now we need to copy the old values to the new buffer.  Two cases
      -- can happen, as exemplified in the following.  Suppose
      -- Old_Len = 4, New_Len = 8 and consider the two cases
      -- First = 2 and First = 6. In the first case (First=2) the old buffer
      -- stores the values f(2)...f(5) and the new buffer will contain
      --
      --    0   | 1  |  2  |  3  |  4 |   5  |  6  |  7
      --   -----+----+-----+-----+----+------+-----+-----
      --   F(8) |F(9)|f(2) |f(3) |f(4)| f(5) |F(6) |F(7)
      --   -----+----+-----+-----+----+------+-----+-----
      --
      -- The entries marked with "F(...)" instead of "f(...)" are
      -- the entries which are false since they are outside the
      -- old buffer.  If First=6 instead the old buffer stores the values
      -- and the new buffer results
      --
      --    0   | 1  |  2  |  3  |  4 |   5  |  6  |  7
      --   -----+----+-----+-----+----+------+-----+-----
      --   f(8) |f(9)|F(10)|F(11)|F(12)|F(13)|f(6) |f(7)   f(8)??  f(9)??
      --   -----+----+-----+-----+----+------+-----+-----
      --
      -- Note that now  the old buffer does not fit in the "tail" of
      -- the new one and part of the old buffer must be copied to the
      -- head
      --
      -- Get the position of the border in the new buffer.
      New_Border := List.First mod New_Len;
      Last_Written := New_Border + Old_Len - 1;

      if (Last_Written <= List.Buffer'Last) then
         -- The whole old buffer fits in the "tail" after New_Border.
         -- Simply copy the old buffer over the new one
         List.Buffer(New_Border..Last_Written) := Old_Buffer;
      else
         -- If we are here, the old buffer does not fit in the tail.
         -- Copy what fits in the tail and the remainder in the
         -- head.
         declare
            Stop : Item_Type := List.Buffer'Last - New_Border;
         begin
            List.Buffer(New_Border..List.Buffer'Last)
              := Old_Buffer(0..Stop);

            List.Buffer(0..Last_Written-List.Buffer'Last) :=
              Old_Buffer(Stop+1..Old_Buffer'Last);
         end;
      end if;
   end Resize_Buffer;

   procedure Insert (List         : in out Integer_List;
                     Item         : in     Item_Type;
                     On_Duplicate : in     Duplicate_Policy := Die)
   is
   begin
      -- Put_Line("insert " & Integer'Image(item));
      -- Put("ingresso : first=" & Integer'Image(List.First));
      -- Dump(List.Buffer.all);

      -- Make sure there is enough room for Item
      if (Item >= List.Buffer'Length+List.First) then
         Resize_Buffer(List, Item);
      end if;

      if (Item < List.First or List.Buffer(Item mod List.Buffer'Length)) then
         -- Item is already registered: we return right away or die
         -- depending on the value of On_Duplicate
         case On_Duplicate is
            when Die =>
               raise Program_Error;
            when Ignore =>
               return;
         end case;
      end if;

      -- Item is not registered: insert it
      if (Item /= List.First) then
         -- The easy case
         List.Buffer (Item mod List.Buffer'Length) := True;
      else
         --
         -- This case is more complex. Since we are inserting the
         -- first element we need to move the border.
         -- We will search for the first value > Item which is not
         -- in the list
         --
         declare
            New_First : Item_Type := List.First+1;
            New_border : Item_Type := New_First mod List.Buffer'Length;
            Border : Item_Type := List.First mod List.Buffer'Length;
         begin
            while (New_Border /= Border) and List.Buffer(New_Border) loop
               -- Move the border one step ahead.  Note that now
               -- the element of position new_border is relative
               -- to an element which is not in the list
               List.Buffer(New_Border) := False;

               New_First := New_First+1;

               New_Border := (New_Border+1) mod List.Buffer'Length;
            end loop;

            List.First := New_First;
         end;
      end if;

      -- Put("uscita : first=" & Integer'Image(List.First)); Dump(List.Buffer.all);

   end Insert;

   function Contains (List : in Integer_List;
                      Item : in Item_Type) return Boolean
   is
   begin
      if (Item < List.First) then
         return True;
      elsif (Item = List.First or Item > List.First + List.Buffer'Length) then
         return False;
      else
         return List.Buffer(Item mod List.Buffer'Length);
      end if;
   end Contains;

   procedure Initialize(Object: in out Integer_List)
   is
   begin
      Object.Buffer := new Item_Buffer'(0..128 => False);
      Object.First  := Item_Type'First;
   end Initialize;
begin
   if (Item_Type'First /= 0) then
      raise Constraint_Error;
   end if;
end Integer_Lists;

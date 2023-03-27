with Text_Io;
use Text_Io;
package body Id_Counters is

   -----------------
   -- New_Counter --
   -----------------

   function New_Counter
     (Dim : Positive)
      return Counter
   is
      Result : Counter;
   begin
      Result := (Current_Top  => 0,
                 Top_Position => 1,
                 Id_Buffer    => new Id'(1..Dim => 0));
      return Result;
   end New_Counter;

   procedure Stampa (C: Counter) is
   begin
      Put_Line("Top Pos : " & Integer'Image(C.Top_Position));
      Put_Line("Top     : " & Integer'Image(C.Id_Buffer (C.Top_Position)));
      Put_Line("ID :");
      for I in C.Id_Buffer'Range loop
         Put(Integer'Image(C.Id_Buffer(I)) & " ");
      end loop;
      Put_Line("");
   end Stampa;

   ----------
   -- Next --
   ----------

   --
   -- How do we advance the ID counter? Easy,
   --
   --   1. We keep track of the "skin" we are generating
   --   2. When we generated all the vectors in a skin, we go
   --      to the next one
   --
   -- How do we generate all the vectors in skin S?  By definition
   -- a vector belongs to skin S if its maximum value is equal to
   -- S.   If n belongs to skin S, call the Top_Position of n the
   -- smallest k such that n_k=S.  In order to generate the vectors
   -- in skin S we
   --
   --  1. Generate all the S-skin vectors with Top_Position=1
   --  2. Generate all the S-skin vectors with Top_Position=2
   --  3. Generate all the S-skin vectors with Top_Position=3
   --  ...and so on...
   --
   -- Every vector in S-skin with Top_Position=T is uniquely determined
   -- by
   --
   --    A) the vector of the first T-1 entries (called the "head").
   --       Note that the entries of the head lie in 0..S-1
   --
   --    B) the vector of the last N-T entries (called the "tail").
   --       Note that the entries of the head lie in 0..S
   --
   -- So, the algorithm for generatin all the (S,T)-vector can be
   -- summarized as follows
   --
   --    1. Advance the head.
   --    2. If the head overflows, advance the tail
   --    3. If the tail overflows, advance the Top_Position
   --    4. If the Top_Position overflows, advance S
   --

   procedure Next (C : in out Counter) is
      type Part is (Head, Tail);

      --
      -- Advance the head or the tail of X by one step.
      -- If entry n overflows (i.e. X(n) becomes greater
      -- than Max), set X(n) to zero and propagate the
      -- carry to the next entry.  Overflow is set to True if
      -- and only if all the entries overflow.  If Overflow is
      -- True, all the entries of X are equal to zero.
      --
      procedure Inc_Vec (X        : in out Id;
                         Top_Idx  : in     Positive;
                         What     : in     Part;
                         Overflow :    out Boolean) is
         From : Natural;  -- First index of subvec to be inc
         To   : Natural;  -- Last  index of subvec to be inc
         Max  : Natural;  -- Max admissible value in the subvec
      begin
         -- First find the first and last location of the
         -- subvector to be incremented
         case What is
            when Head =>
               if (Top_Idx = X'First) then
                  -- No head: overflow by default
                  Overflow := True;
                  return;
               else
                  -- The head goes from 1 to Top_Idx-1
                  -- Every entry in the head is less than the top
                  From := X'First;
                  To   := Top_Idx-1;
                  Max  := X(Top_Idx)-1;
               end if;
            when Tail =>
               if (Top_Idx = X'Last) then
                  -- No tail: overflow by default
                  Overflow := True;
                  return;
               else
                  -- The tail goes from Top_Idx+1 to the end
                  -- Every entry in the head is not larger than the top
                  From := Top_Idx+1;
                  To   := X'Last;
                  Max  := X(Top_Idx);
               end if;
         end case;

         for Idx in From..To loop
            if (X(Idx) < Max) then
               -- Idx-th element does not overflow
               X(Idx) := X(Idx)+1;
               Overflow := False;
               return;
            else
               -- Idx-th element overflows
               X(Idx) := 0;
            end if;
         end loop;

         -- If I am here, every element of X overflowed
         Overflow := True;
      end Inc_Vec;

      Overflow : Boolean;
   begin
      pragma Assert(C.Id_Buffer(C.Top_Position) = C.Current_Top);

      if (C.Current_Top = 0) then
         -- Special case
         C.Current_Top   := 1;
         C.Top_Position  := 1;
         C.Id_Buffer (1) := 1;
         return;
      end if;

      -- First try to increment the head
      Inc_Vec(X        => C.Id_Buffer.all,
              Top_Idx  => C.Top_Position,
              What     => Head,
              Overflow => Overflow);


      if (not Overflow) then
         -- The head did not overflow: nothing more to do
         return;
      end if;

      -- Increment the tail
      Inc_Vec(X        => C.Id_Buffer.all,
              Top_Idx  => C.Top_Position,
              What     => Tail,
              Overflow => Overflow);

      if (not Overflow) then
         -- The tail did not overflow: nothing more to do
         return;
      end if;

      -- Here the tail overflew: if there is some room,
      -- move the top one position ahead, otherwise
      -- we reached the last ID in the current skin,
      -- so we must go to the next one.
      -- Remember that the first ID with a given top is zero
      -- everywhere, but in the first entry which is equal to
      -- the top
      if (C.Top_Position < C.Id_Buffer'Last) then
         -- Some room still available
         C.Id_Buffer(C.Top_Position) := 0;
         C.Top_Position := C.Top_Position + 1;
         C.Id_Buffer(C.Top_Position) := C.Current_Top;
      else
         -- No more room available: go to the next skin
         C.Current_Top := C.Current_Top + 1;

         -- Remove the current top
         C.Id_Buffer(C.Top_Position) := 0;
         C.Id_Buffer(1) := C.Current_Top;
         C.Top_Position := 1;
      end if;


   end Next;

   ----------------
   -- Current_Id --
   ----------------

   function Current_Id
     (C : Counter)
      return Id
   is
   begin
      return C.Id_Buffer.all;
   end Current_Id;

end Id_Counters;

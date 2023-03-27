with Text_Io; use Text_Io;

with Ada.Unchecked_Deallocation;

package body Generic_Packet_Map is

   procedure Delete_Packet_Buffer is
      new Ada.Unchecked_Deallocation(Packet_Buffer, Packet_Buffer_Pt);

   procedure Delete_Packet_Vector is
      new Ada.Unchecked_Deallocation(Packet_Vector, Packet_Vector_Pt);

   use type Pkt_Maps.Cursor;

   procedure Init (Table       : in out Packet_Table;
                   N_Portions : in     Portion_Type := Portion_Type'Last) is
   begin
      if (Table.N_Packets > -1) then
         raise Program_Error;
      end if;

      Table.N_Packets := N_Portions;
   end Init;


   procedure Insert (Table    : in out Packet_Table;
                     Index    : in     Index_Type;
                     Packet   : in     Packet_Type;
                     Portion : in     Portion_type;
                     Inserted :    out Positive) is
      Pos : Packet_Cursor;
      Buf : Packet_Buffer_Pt;
   begin
      -- Check if we already received a packet with this
      -- index
      Pos := Table.Table.Find(Index);
      if (Pos = Pkt_Maps.No_Element) then
         -- This is the first packet with its index.
         -- Create a new entry.
         Buf := new Packet_Buffer'(Buffer =>
                                     new Packet_Vector(1..Table.N_Packets),
                                   Received =>
                                     new Flag_Vector'(1..Table.N_Packets =>
                                                        False),
                                   Packet_Stored => 0);

         Table.Table.Insert(Index, Buf);
      else
         Buf := Pkt_Maps.Element(Pos);
         if (Buf.Received(Portion)) then
            raise Program_Error;
         end if;
      end if;


      Buf.Buffer   (Portion) := Packet;
      Buf.Received (Portion) := True;
      Buf.Packet_Stored := Buf.Packet_Stored+1;
      Inserted := Buf.Packet_Stored;
   end Insert;

   procedure Insert (Table     : in out Packet_Table;
                     Index : in     Index_Type;
                     Packet    : in     Packet_Type;
                     Inserted  :    out Positive)  is
      Pos : Packet_Cursor;
      Buf : Packet_Buffer_Pt;
   begin
      Pos := Table.Table.Find(Index);
      if (Pos = Pkt_Maps.No_Element) then
         -- This is the first time we receive a packet with
         -- this index.  Insert it in the first position
         Insert (Table     => Table,
                 Index => Index,
                 Packet    => Packet,
                 Inserted  => Inserted,
                 Portion   => 1);
      else
         -- Other packets with the same index were received.
         -- Search for the first free entry in Buf.Buffer
         Buf := Pkt_Maps.Element(Pos);

         if (Buf.Packet_Stored = Table.N_Packets) then
            -- N_packets already received: no more room
            raise Table_Overflow;
         end if;

         for I in Buf.Received'Range loop
            if (not Buf.Received(I)) then
               -- Free entry found!
               Insert (Table     => Table,
                       Index => Index,
                       Packet    => Packet,
                       Inserted  => Inserted,
                       Portion   => I);

               -- Our duty is done, we can return.
               return;
            end if;
         end loop;

         -- I should never arrive here, since the condition
         -- before the loop was false.  If am here, there is some
         -- serious bug, so a "raise" is "de rigour"
         raise Program_Error;
      end if;
   exception
      when others => null;
   end Insert;

   function Contains (Table     : Packet_Table;
                      Index : Index_Type) return Boolean is
   begin
      return Table.Table.Find(Index) /= Pkt_Maps.No_Element;
   end Contains;


   function Contains (Table    : Packet_Table;
                      Index    : Index_Type;
                      Portion : Positive) return Boolean is
      Pos : Packet_Cursor := Table.Table.Find(Index);
      Buf : Packet_Buffer_Pt;
   begin
      if (Pos = Pkt_Maps.No_Element) then
         return False;
      end if;

      Buf := Pkt_Maps.Element(Pos);
      return Buf.Received(Portion);
   end Contains;

   function Missing (Table     : Packet_Table;
                     Index : Index_Type) return Portion_Array  is
      Pos : Packet_Cursor := Table.Table.Find(Index);
   begin
      if (Pos = Pkt_Maps.No_Element) then
         -- No packet with this index was received
         declare
            Result : Portion_Array(1..Table.N_Packets);
         begin
            for I in Result'Range loop
               Result (I) := I;
            end loop;

            return Result;
         end;
      else
         -- Some packet was received
         declare
            -- Get the packet buffer
            Buf    : Packet_Buffer_Pt  :=  Pkt_Maps.Element(Pos);

            -- Compute the result length
            Size   : Natural  :=  Table.N_Packets - Buf.Packet_Stored;

            -- Allocate the result vector
            Result : Portion_Array(1..Size);

            N      : Positive := 1;
         begin
            for I in Buf.Received'Range loop
               if (Buf.Received(I)) then
                  Result(N) := I;
                  N := N+1;
               end if;
            end loop;

            pragma Assert(N = Size+1);

            return Result;
         end;
      end if;
   end Missing;


   function N_Stored (Table     : in  Packet_Table;
                      Index : in Index_Type) return Natural is
      Pos : Packet_Cursor := Table.Table.Find(Index);
   begin
      if (Pos = Pkt_Maps.No_Element) then
         return 0;
      else
         return Pkt_Maps.Element(Pos).Packet_stored;
      end if;
   end N_Stored;



   function Get (Table : Packet_Table;
                 Index : Index_Type) return Packet_Vector is
      Pos : Packet_Cursor;
      Buf : Packet_Buffer_Pt;
   begin
      Pos := Table.Table.Find(Index);
      if (Pos = Pkt_Maps.No_Element) then
         declare
            P : Packet_Vector(1..0);
         begin
            return P;
         end;
      else
         Buf := Pkt_Maps.Element(Pos);

         declare
            P : Packet_Vector(1..Buf.Packet_Stored);
            N : Positive := 1;
         begin
            for I in Buf.received'Range loop
               if Buf.Received(I) then
                  P(N) := Buf.Buffer(I);
                  N := N+1;
               end if;
            end loop;

            pragma Assert(N = Buf.Packet_Stored);
            return P;
         end;
      end if;
   end Get;

   function Get (Table   : in Packet_Table;
                 Index   : in Index_Type;
                 Portion : in Portion_type)
                return Packet_Type is
      Pos : Packet_Cursor;
      Buf : Packet_Buffer_Pt;
   begin
      Pos := Table.Table.Find(Index);
      if (Pos = Pkt_Maps.No_Element) then
         raise Not_Found;
      end if;

      Buf := Pkt_Maps.Element(Pos);
      if (not Buf.Received(Portion)) then
         raise Not_Found;
      end if;

      return Buf.Buffer(Portion);
   end Get;

   procedure Remove (Table     : in out Packet_Table;
                     Index : in     Index_Type) is
      Pos : Packet_Cursor;
      Buf : Packet_Buffer_Pt;
   begin
      Pos := Table.Table.Find(Index);
      if (Pos /= Pkt_Maps.No_Element) then
         Buf := Pkt_Maps.Element(Pos);

         Delete_Packet_Vector(Buf.Buffer);
         Delete_Packet_Buffer(Buf);

         Table.Table.Delete(Pos);
      end if;
   end Remove;
end Generic_Packet_Map;

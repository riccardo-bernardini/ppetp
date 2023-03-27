with Ada.Unchecked_Deallocation;

with System.Storage_Elements;
use  System.Storage_Elements;

with System.Address_To_Access_Conversions;


package body Generic_Tables is
   procedure Free  is
      new Ada.Unchecked_Deallocation (Object => Buffer_Array,
                                      Name   => Buffer_Pt);

   -- function To_Str (X : Buffer_Pt) return String is
   --    package Conv is
   --       new System.Address_To_Access_Conversions(Buffer_Array);
   --    Tmp : System.Address;
   -- begin
   --    Tmp := Conv.To_Address(Conv.Object_Pointer(X));
   --    return Integer_Address'Image(To_Integer(Tmp));
   -- end To_Str;


   protected body Table is
      procedure Reserve (Index     :    out Cursor) is
         Idx : Cursor;
      begin


         if Buf.Buffer = null then
            raise Uninitialized_Table;
         end if;

         Idx  := Buf.Buffer'First;
         if (Buf.N_Busy = Buf.Size) then
            raise Table_Full;
         end if;

         while (Idx <= Buf.Buffer'Last
                and then Buf.Buffer(Idx).Busy) loop
            Idx := Idx + 1;
         end loop;

         if (Idx > Buf.Buffer'Last) then
            -- This should never happen
            raise Program_Error;
         else
            Buf.Buffer(Idx).Busy := True;
            Buf.N_Busy := Buf.N_Busy + 1;
            Index := Idx;
         end if;

      end Reserve;

      procedure Insert (Item      : in     Element;
                        Index     :    out Cursor) is
         Idx : Cursor;
      begin
         Reserve(Idx);
         Buf.Buffer(Idx).Data := Item;
         Index := Idx;
      end Insert;


      function Get (Index     : Cursor)
                   return Element is
      begin
         if (Buf.Buffer(Index).Busy) then
            return Buf.Buffer(Index).Data;
         else
            raise Invalid_Cursor;
         end if;
      end Get;

      procedure Replace (Index    : in Cursor;
                         New_Item : in Element) is
      begin
         if (Buf.Buffer(Index).Busy) then
            Buf.Buffer(Index).Data := New_Item;
         else
            raise Invalid_Cursor;
         end if;
      end Replace;


      procedure Iterate (Callback : access procedure (X : Element)) is
         N_Processed : Natural := 0;
      begin
         if (Buf.N_Busy = 0) then
            return;
         end if;

         for I in Buf.Buffer'Range loop
            if Buf.Buffer(I).Busy then
               Callback(Buf.Buffer(I).Data);
               N_Processed := N_Processed + 1;

               if (N_Processed = Buf.N_Busy) then
                  return;
               end if;
            end if;
         end loop;
      end Iterate;

      procedure Iterate (Callback : Callback_Function) is
         N_Processed : Natural := 0;
      begin
         if (Buf.N_Busy = 0) then
            return;
         end if;

         for I in Buf.Buffer'Range loop
            if Buf.Buffer(I).Busy then
               Buf.Buffer(I).Data := Callback(Buf.Buffer(I).Data);
               N_Processed := N_Processed + 1;

               if (N_Processed = Buf.N_Busy) then
                  return;
               end if;
            end if;
         end loop;
      end Iterate;

      procedure Start_Iteration is
      begin
         Buf.Iter_Cursor := Cursor_Iter_Type(Buf.Buffer'First-1);
      end Start_Iteration;

      function  Iterate_Again return Boolean is
      begin
         for I in Cursor(Buf.Iter_Cursor + 1) .. Buf.Buffer'Last loop
            if (Buf.Buffer (I).Busy) then
               return True;
            end if;
         end loop;

         return False;
      end Iterate_Again;

      procedure Next_Iteration (Item : out Element) is
      begin
         for I in Cursor(Buf.Iter_Cursor + 1) .. Buf.Buffer'Last loop
            if (Buf.Buffer (I).Busy) then
               Item := Buf.Buffer (I).Data;
               Buf.Iter_Cursor := Cursor_Iter_Type(I);
               return;
            end if;
         end loop;

         raise Constraint_Error;
      end Next_Iteration;

      procedure Delete (Index : Cursor) is
      begin
         if (Buf.Buffer(Index).Busy) then
            Buf.Buffer(Index).Busy := False;
            Buf.N_Busy := Buf.N_Busy - 1;

         else
            raise Invalid_Cursor;
         end if;
      end Delete;


      procedure Delete_If (Callback: Boolean_Callback_Function) is
      begin
         if (Buf.N_Busy = 0) then
            return;
         end if;

         for I in Buf.Buffer'Range loop
            if Buf.Buffer(I).Busy then
               if Callback(Buf.Buffer(I).Data) then
                  Buf.Buffer(I).Busy := False;
                  Buf.N_Busy := Buf.N_Busy - 1;
               end if;
            end if;
         end loop;
      end Delete_If;


      function Is_Full return Boolean is
      begin
         return Buf.N_Busy = Buf.Size;
      end Is_Full;

      function Is_Empty return Boolean is
      begin

         if Buf.Buffer = null or Buf.N_Busy = 0 then
            return true;
         else
            return false;
         end if;
      end Is_Empty;

      function Exists (Index : Cursor) return Boolean is
      begin
         return (Buf.Buffer(Index).Busy);
      end Exists;

      procedure Resize (Size : Size_Type) is
         Last : Cursor := Cursor'First+Cursor(Size-1);

      begin
         Free(Buf.Buffer);
         Buf.Size := Size;
         Buf.N_Busy := 0;
         Buf.Buffer := new Buffer_Array(Cursor'First..Last);
      end Resize;


      function Count return Size_Type is
      begin
         return Buf.N_Busy;
      end Count;

   end Table;




   overriding procedure Finalize (Object : in out Basic_Table) is
   begin
      Free (Object.Buffer);
   end Finalize;
end Generic_Tables;

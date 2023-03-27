with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Streams;       use Ada.Streams;
with Smart_Records;     use Smart_Records;
with Ada.Finalization;  use Ada.Finalization;

package body Parsing_Buffers is

   procedure Finalize (X : in out Parsing_Buffer) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Byte_Array,
                                        Name   => Byte_Array_Pt);
   begin
      Free (X.Data);
   end Finalize;

   -------------------------
   -- Make_Parsing_Buffer --
   -------------------------

   function Make_Parsing_Buffer (Packet : Byte_Array)
                                 return Parsing_Buffer is
   begin
      return Result : Parsing_Buffer do
         Result.Data   := new Byte_Array'(Packet);
         Result.Cursor := Packet'First;
         Result.Last   := Packet'Last;
      end return;
   end Make_Parsing_Buffer;



   --     Versione Modificata Da AS: --

--     function Make_Parsing_Buffer (Packet : Byte_Array)
--                                   return Parsing_Buffer is
--        Count : Byte_Array_Offset := 1;
--     begin
--        return Result : Parsing_Buffer do
--           Result.Data   := new Byte_Array(1 .. Packet'Last - Packet'First); --'(Packet);
--           for I in Packet'First .. Packet'Last
--             loop
--              Result.Data(Count) := Packet(I);
--              Count := Count +1;
--              end loop;
--           Result.Cursor := 1;
--           Result.Last   := Packet'Last -Packet'First;
--        end return;
--     end Make_Parsing_Buffer;


   -------------------
   -- Get_Remaining --
   -------------------

   procedure Get_Remaining (Buffer : in out Parsing_Buffer;
                            Data   :    out Target) is
   begin
      Data := new Byte_Array'
        (Buffer.Data (Buffer.Cursor .. Buffer.Last));
      Buffer.Cursor := Buffer.Last + 1;
   end Get_Remaining;

   ----------
   -- Fill --
   ----------

   procedure Fill (Src : in out Parsing_Buffer;
                   Dst :    out Byte_Array) is
   begin
      Src.Remain_At_Least_Or_Die (Dst'Length);
      Dst := Src.Data (Src.Cursor .. Src.Cursor + Dst'Length - 1);
      Src.Cursor := Src.Cursor + Dst'Length;
   end Fill;

   ---------------------------
   -- Remain_Exactly_Or_Die --
   ---------------------------
   procedure Remain_Exactly_Or_Die (Where    : Parsing_Buffer;
                                    How_Many : Byte_Array_Offset) is
   begin
      if (Where.Last - Where.Cursor + 1 /= How_Many) then
         raise Length_Constraint;
      end if;
   end Remain_Exactly_Or_Die;
   pragma Inline (Remain_Exactly_Or_Die);

   ----------------------------
   -- Remain_At_Least_Or_Die --
   ----------------------------
   procedure Remain_At_Least_Or_Die (Where    : Parsing_Buffer;
                                     How_Many : Byte_Array_Offset) is
   begin
      if (Where.Last - Where.Cursor + 1 < How_Many) then
         raise Length_Constraint;
      end if;
   end Remain_At_Least_Or_Die;
   pragma Inline (Remain_At_Least_Or_Die);

   ----------------------
   -- Die_If_Not_Empty --
   ----------------------
   procedure Die_If_Not_Empty (What : Parsing_Buffer) is
   begin
      if (What.Last + 1 /= What.Cursor) then
         raise Length_Constraint;
      end if;
   end Die_If_Not_Empty;
   pragma Inline (Die_If_Not_Empty);

   -------------
   -- Extract --
   -------------
   procedure Extract (Packet : in out Parsing_Buffer;
                      Result :    out Target) is

      Size : constant Byte_Array_Offset :=
               Target'Size / Stream_Element'Size;

      subtype Data_Buffer is Byte_Array (1 .. Size);

      function Basic_Conversion is
        new Ada.Unchecked_Conversion (Source => Data_Buffer,
                                      Target => Target);
   begin
      Remain_At_Least_Or_Die (Where    => Packet,
                              How_Many => Size);

      Result := Basic_Conversion
        (Packet.Data (Packet.Cursor .. Packet.Cursor + Size - 1));

      Packet.Cursor := Packet.Cursor + Size;
   end Extract;

   -----------------
   -- Remove_Last --
   -----------------
   procedure Remove_Last (Buffer   : in out Parsing_Buffer;
                          How_Many : in     Byte_Array_Offset)
   is
   begin
      Remain_At_Least_Or_Die (Buffer, How_Many);
      Buffer.Last := Buffer.Last - How_Many;
   end Remove_Last;

   ----------
   -- Skip --
   ----------
   procedure Skip (Buffer   : in out Parsing_Buffer;
                   How_Many : in     Byte_Array_Offset)
   is
   begin
      Remain_At_Least_Or_Die (Buffer, How_Many);
      Buffer.Cursor := Buffer.Cursor + How_Many;
   end Skip;

   ----------
   -- Data --
   ----------
   function Data (Buffer : Parsing_Buffer;
                  Pos    : Byte_Array_Offset)
                  return Stream_Element
   is
   begin
      return Buffer.Data (Pos);
   end Data;
   pragma Inline (Data);

   ---------------------
   --    GET_ACTUAL   --
   ---------------------
   -- NOTE: this function has not been tested yet --

   function Get_Actual (Buffer : Parsing_Buffer)
                           return Byte is
   begin
      return Buffer.Data(Buffer.Cursor);
   end Get_Actual;
   pragma Inline (Get_Actual);



   ----------
   -- Last --
   ----------
   function Last (Buffer : Parsing_Buffer)
                  return Byte_Array_Offset is
   begin
      return Buffer.Last;
   end Last;


   -------------------
   -- Actual_Cursor --
   -------------------
   function Actual_Cursor (Buffer : Parsing_Buffer)
                           return Byte_Array_Offset is
   begin
      return Buffer.Cursor;
   end Actual_Cursor;


   --------------
   -- Set_Last --
   --------------
   procedure Set_Last (Buffer : in out Parsing_Buffer;
                       Last   : in     Byte_Array_Offset) is
   begin
      Buffer.Last := Last;
   end Set_Last;

   ----------------
   -- Set_Actual --
   ----------------
   procedure Set_Actual (Buffer : in out Parsing_Buffer;
                         Actual : in     Byte_Array_Offset) is
   begin
      Buffer.Cursor := Actual;
   end Set_Actual;



   ---------------
   -- Remaining --
   ---------------
   function Remaining (Buffer : Parsing_Buffer)
                       return Byte_Array_Offset is
   begin
      pragma Assert (Buffer.Last - Buffer.Cursor + 1 >= 0);
      return Buffer.Last - Buffer.Cursor + 1;
   end Remaining;


end Parsing_Buffers;

with Ada.Unchecked_Deallocation;

package body Generic_Shared_Queue is

   protected body Queue_Handler is
      ------------
      -- Insert --
      ------------

      procedure Insert (What  : in     Element)
      is
      begin
         Buffer.Append(What);
      end Insert;

      -------------
      -- Extract --
      -------------

      entry Extract (Head : out Element) when not Buffer.Is_Empty
      is
      begin
         Head := Buffer.First_Element;
         Buffer.Delete_First;
      end Extract;

      procedure Extract_Now (Head : out Element)
      is
      begin
         if Buffer.Is_Empty then
            raise Empty_Queue;
         end if;

         Head := Buffer.First_Element;
         Buffer.Delete_First;
      end Extract_Now;
      --------------
      -- Is_Empty --
      --------------

      function Is_Empty  return Boolean is
      begin
         return Buffer.Is_Empty;
      end Is_Empty;

      ----------
      -- Peek --
      ----------

      function Peek return Element is
      begin
         if (Is_Empty(Buffer)) then
            raise Empty_Queue;
         else
            return Buffer.First_Element;
         end if;
      end Peek;
   end Queue_Handler;

   ---------------
   -- New_Queue --
   ---------------

   function New_Queue return Queue_Pt is
   begin
      return new Queue_Handler;
   end New_Queue;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Queue_Pt) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Queue_Handler, Queue_Pt);
   begin
      Free(X);
   end Finalize;
end Generic_Shared_Queue;

package body Generic_Indefinite_Queue is

   ------------
   -- Insert --
   ------------

   procedure Insert (Queue : in out FIFO_Queue;
                     What  : in     Element)
   is
   begin
      Queue.Buffer.Append(What);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Queue : in FIFO_Queue) return Boolean is
   begin
      return Is_Empty (Queue.Buffer);
   end Is_Empty;

   ----------
   -- Peek --
   ----------

   function Peek (Queue : in FIFO_Queue) return Element is
   begin
      if (Is_Empty(Queue.Buffer)) then
         raise Empty_Queue;
      else
         return First_Element(Queue.Buffer);
      end if;
   end Peek;

   -------------
   -- Extract --
   -------------

   procedure Extract (Queue : in out FIFO_Queue;
                      Head  :    out Element)
   is
   begin
      if (Is_Empty(Queue.Buffer)) then
         raise Empty_Queue;
      else
         Head := First_Element(Queue.Buffer);
         Delete_First(Queue.Buffer);
      end if;
   end Extract;

end Generic_Indefinite_Queue;

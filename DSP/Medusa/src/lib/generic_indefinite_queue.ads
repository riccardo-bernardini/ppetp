with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Element(<>) is private;
   with function "=" (Left, Right : Element) return Boolean is <>;
package Generic_Indefinite_Queue is
   type FIFO_Queue is tagged private;
   procedure Insert (Queue : in out FIFO_Queue;
                     What  : in     Element);

   function Is_Empty (Queue : in FIFO_Queue) return Boolean;

   function Peek (Queue : in FIFO_Queue) return Element;

   procedure Extract (Queue : in out FIFO_Queue;
                      Head  :    out Element);

   Empty_Queue : exception;
private
   package Element_Lists is
      new  Ada.Containers.Indefinite_Doubly_Linked_Lists(Element);
   use Element_Lists;

   type FIFO_Queue is tagged record
      Buffer : List := Element_Lists.Empty_List;
   end record;
end Generic_Indefinite_Queue;

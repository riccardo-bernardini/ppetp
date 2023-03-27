--                              -*- Mode: Ada -*-
--  Filename        : generic_protected_queue.ads
--  Description     : Generic buffer for task communication
--  Author          : Riccardo Bernardini
--  Created On      : Mon Feb 11 13:34:05 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- The tasks in the MMEDIA module of Medusa communicate through "queues
-- of packets".  This little generic package implements a generic
-- protected queue (implemented as a protected type Queue_Type).
-- A Queue_Type has only two methods:
--
--    procedure Insert  (What : in     Element_Type);
--    entry     Extract (What : in out Element_Type);
--
-- As expected, entry Extract waits for a non-empty queue.
--
-- Usage Example:
--
--    package Integer_Queues is
--       new Generic_Protected_Queue(Element_Type => Integer);
--
--    Queue  : Integer_Queues.Queue_Type;
--
--    Result : Integer;
--
--    Queue.Insert(42);
--    Queue.Extract(Result);
--



with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;
package Generic_Protected_Queue is
   package Element_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => Element_Type);

   protected type Queue_Type is
      procedure Insert  (What : in     Element_Type);
      entry     Extract (What : in out Element_Type);
   private
      Buffer : Element_Lists.List;
   end Queue_Type;

   type Access_Queue_Type is access all Queue_Type;


end Generic_Protected_Queue;

--                              -*- Mode: Ada -*-
--  Filename        : generic_shared_queue.ads
--  Description     : Generic package for a shared queue
--  Author          :
--  Created On      : Mon Sep  8 11:57:12 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides a protected type which implements a FIFO
-- queue which can be used for data exchange between tasks. This
-- package must be instantiated with the type of the queue entries and
-- with a "=" function which implements the equality between queue
-- entries.  The queue size is unbounded (within the computer memory
-- constraints, of course)
--
--
with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element is private;
   with function "=" (Left, Right : Element) return Boolean is <>;
package Generic_Shared_Queue is
   package Element_Lists is
      new  Ada.Containers.Doubly_Linked_Lists(Element);
   use Element_Lists;

   protected type Queue_Handler is
      procedure Insert (What : in Element);
      -- Insert element What in the queue

      entry Extract (Head : out Element);
      -- Extract the first element of the queue.  If the queue is
      -- empty, wait until at least one element is in the queue.

      procedure Extract_Now (Head : out Element);
      -- As Extract, but do not wait for a non-empty queue.  If
      -- the queue is empty, raise a Empty_Queue exception.

      function Is_Empty return Boolean;
      -- Return true if the queue is empty.

      function Peek     return Element;
      -- Return the first element of the queue, but do not extract it
      -- (so that next call to Extract and further calls to Peek
      -- without any Extract in beetwen will return the same
      -- element). If the queue is empty, raise Empty_Queue
      -- exception.
   private
      Buffer : List := Element_Lists.Empty_List;
   end Queue_Handler;

   Empty_Queue : exception;

   type Queue_Pt is access Queue_Handler;

   function New_Queue return Queue_Pt;

   procedure Finalize (X : in out Queue_Pt);
end Generic_Shared_Queue;

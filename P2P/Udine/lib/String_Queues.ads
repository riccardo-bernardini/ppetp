with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Generic_Protected_queue;

package String_Queues is
   new Generic_Protected_Queue(Element_Type => Unbounded_String);


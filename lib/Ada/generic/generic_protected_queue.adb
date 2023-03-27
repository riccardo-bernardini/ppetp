package body Generic_Protected_Queue is
   protected body Queue_Type is
      procedure Insert  (What : in Element_Type) is
      begin
         Buffer.Append(What);
      end Insert;

      entry Extract (What : in out Element_Type) when not Buffer.Is_Empty is
      begin
         What := Buffer.First_Element;
         Buffer.Delete_First;
      end Extract;
   end Queue_Type;
end Generic_Protected_Queue;

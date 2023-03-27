package body Hash_Auth is

   Failed : exception;

   -- Definizione delle funzioni Register e Process

   procedure Register (Timestamp  : in Unbounded_String;
                       Parameters : in Auth_Data) is
   begin
      T.Memory_Table.Insert(Timestamp, Parameters);
   end Register;

   function Process (Timestamp : in Unbounded_String) return Auth_Data is
      Q : Auth_Data;

   begin

      if  Find(T.Memory_Table, Timestamp) = No_Element then
      raise Failed with "Wrong Timestamp";
         else Q := Element(T.Memory_Table, Timestamp);

      end if;

      return Q;

   end Process;

end Hash_Auth;

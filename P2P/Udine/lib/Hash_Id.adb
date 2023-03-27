package body Hash_Id is

   Failed : exception;

   -- Definizione delle funzioni Register e Process

   procedure Register_Id (ID         : in Unbounded_String;
                          Parameters : in Id_Data) is
   begin
      T.Client_Table.Insert(ID, Parameters);
   end Register_Id;

   function Process_Id (Id : in Unbounded_String) return Id_Data is
      Q : Id_Data;

   begin

      if  Find(T.Client_Table, ID) = No_Element then

         return No_Id;

         else Q := Element(T.Client_Table, ID);

      end if;

      return Q;

   end Process_Id;

end Hash_Id;

package body Generic_Parser is
   -- Definizione delle funzioni Register e Process

   procedure Register (P        : in out Parser;
                       Command  : in     Unbounded_String;
                       Callback : in     Callback_Function) is
   begin
      P.Command_Table.Insert(Command, Callback);
   end Register;

   procedure Process (P            : in Parser;
                      Command_Line : in Unbounded_String) is
      Tokens : Token_Array := Tokenize(Command_Line);
   begin

      --  La funzione Find va a cercare Command_Line nella mappa creata,
      --  e ritorna un Cursor. Se la funzione di callback contraddistinta
      --  dal comando Command_Line non è presente, ritorna No_Element,
      --  altrimenti ritorna il puntatore alla funzione corrispondente.
      --  La mancata presenza del comando comporta il sollevamento di un
      --  eccezione.


      if  Find (P.Command_Table, Tokens(Tokens'First)) = No_Element then
         raise Failed with "Wrong Command";
      elsif Tokens'Last < 3 then
         raise Failed with "Wrong Length";
      else
         declare
            Callback : Callback_Function :=
              Element (P.Command_Table, Tokens(Tokens'First));
         begin
            Callback (Name_Command => Tokens(Tokens'First),
                      Timestamp    => Tokens(Tokens'First +1),
                      Parameters   => Tokens(Tokens'First +2..Tokens'Last));
         end;
      end if;
   end Process;
end Parsers;



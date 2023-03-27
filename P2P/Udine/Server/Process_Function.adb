
package body Process_Function is

   Sfida          : Unbounded_String;
   Password       : Unbounded_String;
   Username       : Unbounded_String;
   N_Port         : String(1..5);
   N_Port_Nat     : Port_Type;
   Timestamp_req  : Unbounded_String;
   Group          : constant String := "127.0.0.1";
   Failed         : exception;
   Address        : Unbounded_String;
   C_Timestamp    : Unbounded_String;
   URL            : Unbounded_String;
   Host_A         : Unbounded_String;
   --http://127.0.0.1:55505/ciao

   procedure Strt_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array) is

   begin

      --  A questo punto devo procedere con l'operazione di autenticazione

      Put_Line("Avvio della procedura per la creazione della stringa di autenticazione");

      --  La prima cosa da fare consiste nell'inserire in una tabella di tipo
      --  Auth_Hash i valori del timestamp ricevuto e i vari campi che
      --  completano l'Auth_Array.

      Sfida := To_Unbounded_String("FFFFFF");

      C_Timestamp := Current_Timestamp;
      Put_Line(To_String(C_Timestamp));

      Register(C_Timestamp, (Sfida       => Sfida,
                             URL         => Parameters(Parameters'First),
                             UDP_Data    => Parameters(Parameters'First + 1),
                             UDP_Control => Parameters(Parameters'First + 2),
                             N_Flussi    => Parameters(Parameters'First + 3)));





     --  La struttura della stringa per il messaggio AUTH è la seguente:
     --
     --             AUTH | Timestamp | Sfida | CRC
     --
     --  AUTH      : è una stringa lunga 4 caratteri.
     --  Timestamp : è un numero esadecimale a 6 cifre, ovvero 24 bit.
     --	     (le cifre esadecimali sono sempre maiuscole 'A'....'F').
     --  Sfida     : è un numero esadecimale a 6 cifre.
     --  CRC	    : è una stringa esadecimale di lunghezza 8, ovvero composta
     --	     da 32 bit.

     --  Individuazione del numero della porta UDP.

     N_Port     := To_String(Parameters(Parameters'First + 2));
     N_Port_Nat := Port_Type'Value(N_Port);

     --  Invio della stringa di AUTH al Client.

     Send_Command("AUTH", (1 => Sfida),(Family => Family_Inet,
                                        Addr   => Inet_Addr(Group),
                                        Port   => N_Port_Nat));


   end Strt_Process;


   procedure Rply_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array) is
   begin

      --  La funzione Rply_Process può essere chiamata nei seguenti 3 casi:
      --
      --  1) Risposta alla richiesta di autenticazione (AUTH);
      --  2) Risposta alla richiesta di una lista di flussi (LIST);
      --  3) Risposta alla richiesta di presentazione tra i nodi (HELO).
      --
      --  Il primo passo da fare quindi sarà quello di discriminare
      --  le tre funzioni da eseguire in base al comando richiesto.

      --  Dovrò crearmi poi 3 funzioni esterne che gestiranno opportunamente
      --  la risposta da inoltrare al nodo.

      if Parameters(Parameters'First) = To_Unbounded_String("AUTH") then
         Rply_Auth(Parameters(Parameters'First + 1), Parameters);
      elsif Parameters(Parameters'First) = To_Unbounded_String("LIST") then
         Rply_List;
      elsif Parameters(Parameters'First) = To_Unbounded_String("HELO") then
         Rply_Helo;
      else raise Failed with "Wrong Command";
      end if;

      Put_Line("Ritorno da Process Function");



   end Rply_Process;

end Process_Function;

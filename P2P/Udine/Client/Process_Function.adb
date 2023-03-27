
package body Process_Function is

   Sfida          : Unbounded_String;
   Password       : Unbounded_String;
   Username       : Unbounded_String;
   Total          : String(1..17);
   MD5	          : String(1..32);
   Server_Port    : Port_Type;
   Timestamp_req  : Unbounded_String;
   Group          : constant String := "127.0.0.1";
   Failed         : exception;
   Address        : Unbounded_String;
   C_Timestamp    : Unbounded_String;
   URL            : Unbounded_String;
   URL_Parameters : URL_Info;
   MMedia_Port	  : Integer;
   Host_A         : Unbounded_String;
   --http://127.0.0.1:55505/ciao

   procedure Auth_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array) is
   begin

      --  Creazione di una stringa di sfida costituita dai seguenti campi

      --               Username | sfida | password

      --  Sfida me la ricavo dal campo parametri.




      Username      := To_Unbounded_String("PIPPO");
      Sfida         := Parameters(Parameters'First);
      Timestamp_req := Timestamp;
      Password      := To_Unbounded_String("CIAO");

      Total         := To_String(Username) & " " & To_String(Sfida)
                       & " " & To_String(Password);
      Put_Line(Total);
      MD5_Function(Total, MD5);

      --  Invio della stringa di RPLY al Server.

      Send_Command("RPLY", ( 1 => Name_Command,
                             2 => Timestamp_req,
                             3 => Username,
                             4 => To_Unbounded_String(MD5)
                               ),  (Family => Family_Inet,
                                    Addr   => Inet_Addr(Group),
                                    Port   => 55505));

   end Auth_Process;

  procedure Play_Process(Name_Command : in Unbounded_String;
                         Timestamp    : in Unbounded_String;
                         Parameters   : in Token_Array) is
   begin

      Put_Line("Ciao");
      URL            := Parameters(Parameters'First);
      URL_Parameters := Parse(URL);
      Server_Port    := URL_Parameters.Port;
      Host_A         := URL_Parameters.Host;
      Put_Line(To_String(Host_A));
      Put_Line("Porta");
      MMedia_Port    := Get_Int (Connection => DB,
                                 Var_Name   => "MMEDIA.INTERNAL_PORT");
      Put_Line("Ciao");
      Put_Line("Ciao_1");

      Send_Command("STRT", ( 1 => URL,
                             2 => To_Unbounded_String(Integer'Image(MMEdia_Port)),
                             3 => To_Unbounded_String(Integer'Image(UDP_Port)),
                             4 => To_Unbounded_String("3")
                               ),  (Family => Family_Inet,
                                    Addr   => Inet_Addr(To_String(Host_A)),
                                    Port   => Server_Port));


      Put_Line("CIAO");

   end Play_Process;


   procedure Id_Process(Name_Command : in Unbounded_String;
                        Timestamp    : in Unbounded_String;
                        Parameters   : in Token_Array) is

   begin

      Put_Line("CIAO");

   end Id_Process;
   procedure Send_Process(Name_Command : in Unbounded_String;
                          Timestamp    : in Unbounded_String;
                          Parameters   : in Token_Array) is
   begin

      MMedia_Port    := Get_Int (Connection => DB,
                                 Var_Name   => "MMEDIA.INTERNAL_PORT");

      Send_Command("SEND", ( 1 => Parameters(Parameters'First),
                             2 => Parameters(Parameters'First + 1)
                            ),  (   Family => Family_Inet,
                                    Addr   => Inet_Addr(To_String(Host_A)),
                                    Port   => Port_Type(MMedia_Port)),
                                    Socket_Stream );

   end Send_Process;

end Process_Function;

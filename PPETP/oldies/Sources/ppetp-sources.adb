with Command_Queues;

with Input, Network;
use  Input, Network;


package body PPETP.Sources is
   function New_Source (ID    : Source_ID;
                        Queue : Command_Queues.Queue_Pt;
                        Port  : Port_Type;
                        Peer  : Inet_Addr_Type := Any_Inet_Addr)
                        return Source is
      Result : Source;
      New_Reader : Reader_Task := new Reader(ID, Queue);
      Prt : Port_Type := Port;
   begin

      New_Reader.Open(Prt);
      if (Prt = No_Port) then
         raise Open_Failed;
      end if;


      Result :=  new Source_Handler'(Input_Task => New_Reader,
                                     Port       => Prt,
                                     Status     => Closed);

      Accept_From(Result, Peer);

      return Result;
   end New_Source;

   procedure Set_Timeout (Src     : Source;
                          Timeout : Duration) is
   begin
      Src.Input_Task.Set_Timeout(Timeout);
   end Set_Timeout;

   procedure Close (Src : Source) is
   begin
      Src.Input_Task.Close;
      Src.Status := Closed;
   end Close;

   procedure Accept_From (Src  : Source;
                          Peer : Inet_Addr_Type) is
   begin
      Src.Input_Task.Accept_From(Peer);
      Src.Status := Listening;
   end Accept_From;


   procedure Pause(Src : Source;
                   Action: Pause_Type) is
   begin
      Src.Input_Task.Pause(Action);
   end Pause;



   function Port (Src : Source)
                 return Port_Type is
   begin
      return Src.Port;
   end Port;
end PPETP.Sources;

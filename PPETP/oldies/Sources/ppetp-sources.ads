with Command_Queues;

with Network, Input;
use  Network, Input;

package PPETP.Sources is
   Open_Failed : exception;

   type Source_Handler is limited private;

   type Source is access Source_Handler;

   function New_Source (ID    : Source_ID;
                        Queue : Command_Queues.Queue_Pt;
                        Port  : Port_Type;
                        Peer  : Inet_Addr_Type := Any_Inet_Addr)
                        return Source;

   function Port (Src : Source)
                 return Network.Port_Type;

   procedure Set_Timeout (Src     : Source;
                          Timeout : Duration);

   procedure Close (Src : Source);

   procedure Accept_From (Src  : Source;
                          Peer : Network.Inet_Addr_Type);

   procedure Pause(Src : Source;
                   Action: Pause_Type);

private
   type Source_Status is (Closed, Listening);

   type Source_Handler is
      record
	-- Session    : Session_ID;
         Input_Task : Reader_Task;
         Port       : Network.Port_Type;
         Status     : Source_Status;
      end record;
end PPETP.Sources;

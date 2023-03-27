--                              -*- Mode: Ada -*-
--  Filename        : packets-binary-application.ads
--  Description     : Packets used in the interface with the application
--  Author          : Riccardo Bernardini
--  Created On      : Tue Nov 4 2008 21:52:00
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <FULLY TESTED> (Nov 4 2008 21:52:00)

--
-- This package provides type and procedure for handling the
-- data type used as interface between PPETP and the application
-- level.  Application_Packet is the type of the packets that are
-- inserted into the queue which holds the packets to be
-- delivered to the application level.  Moreover, the packets
-- received from the application level are converted to
-- Application_Packet before deleviring them to the Session.
--

with Byte_Arrays;            use Byte_Arrays;

with PPETP;

package Packets.Binary.Application is
   type Application_Packet is new Binary_Packet with private;

   function New_Packet (Sequence_Num : PPETP.Data_Sequence_Number;
                        StreamID     : PPETP.Stream_ID;
                        Data         : Byte_Array)
                       return Application_Packet;

   function Sequence_Num (Packet : Application_Packet)
                          return PPETP.Data_Sequence_Number;

   function StreamID (Packet : Application_Packet)
                       return PPETP.Stream_ID;

   function "=" (Left, Right : Application_Packet)
                 return Boolean;
private
   type Application_Packet is
     new Binary_Packet With
      record
         Sequence_Num : PPETP.Data_Sequence_Number;
         StreamID     : PPETP.Stream_ID;
      end record;
end Packets.Binary.Application;


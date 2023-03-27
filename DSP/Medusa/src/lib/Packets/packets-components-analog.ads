--                              -*- Mode: Ada -*-
--  Filename        : packets-components-analog.ads
--  Description     : Definition of analog reduced/complete components
--  Author          : Riccardo Bernardini
--  Created On      : Fri Jun 27 16:22:55 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Analog_Data;

package Packets.Components.Analog is
   type Analog_Component (Size : Natural) is
     new Abstract_Component with record
        -- Packet data
        Payload : Analog_Data.Vector(1..Size);
     end record;

   type Analog_Reduced_Component (Size : Natural) is
     new Abstract_Reduced_Component with record
        -- Packet data
        Payload : Analog_Data.Vector(1..Size);
     end record;
end Packets.Components.Analog;

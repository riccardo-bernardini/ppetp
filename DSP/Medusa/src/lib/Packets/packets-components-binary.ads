--                              -*- Mode: Ada -*-
--  Filename        : packets-components-binary.ads
--  Description     : Definition of binary reduced/complete components
--  Author          : Riccardo Bernardini
--  Created On      : Fri Jun 27 16:23:07 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Binary_Data;

package Packets.Components.Binary is
   type Binary_Component (Size : Natural) is
     new Abstract_Component with record
      -- Packet data
      Payload : Binary_Data.Vector (1..Size);
     end record;

   type Binary_Reduced_Component (Size : Natural) is
     new Abstract_Reduced_Component with record
        -- Packet data
        Payload : Binary_Data.Vector(1..Size);
     end record;
end Packets.Components.Binary;

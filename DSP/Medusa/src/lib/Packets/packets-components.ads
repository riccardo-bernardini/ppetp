--                              -*- Mode: Ada -*-
--  Filename        : packets-components.ads
--  Description     : Root package for component types
--  Author          : Riccardo Bernardini
--  Created On      : Fri Jun 27 16:10:20 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- In MEDUXA there are several types of components: analog/binary
-- components and reduced/complete ones.  This package introduces
-- the abstract version of reduced and complete components which
-- will be specialized to the corresponding binary/analog in child
-- packages.  Letting analog/binary components sharing a common
-- ancestor could be useful since it would allow to use
-- class-wide types.
--

with Timestamps;
use  Timestamps;


package Packets.Components is
   type Component_Class    is (Analog, Binary);
   type Component_IDX_Type is new Integer 0..2**16-1;
   type Reduction_IDX      is mod 2**32;

   type Component_Timestamp is
     new Basic_Packet_Timestamp with
      record
         Component_Number : Component_IDX_Type;
      end recordl

   type Abstract_Component is abstract tagged
      record
         ID           : Component_Timestamp;
         Class        : Component_Class;
         Network_Info : Network_Info_Type;
      end record;

   type Component_Class_Access is
     access all Abstract_Component'Class;

   type Component_Array is
     array(Natural range <>) of Component_Class_Access;

   type Abstract_Reduced_Component is
     abstract new Abstract_Component with
      record
         Reduction_vector : Reduction_IDX;
      end record;
end Packets.Components;


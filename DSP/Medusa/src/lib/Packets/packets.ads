--                              -*- Mode: Ada -*-
--  Filename        : packets.ads
--  Description     : Root package (empty) for the "packets" hierarchy
--  Author          : Riccardo Bernardini
--  Created On      : Fri Jun 27 16:24:00 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!
with Timestamps;
use  Timestamps;

package Packets is
   type Basic_Packet_Timestamp is tagged
      record
         Timestamp : Multimedia_Timestamp;
      end record;
end Packets;

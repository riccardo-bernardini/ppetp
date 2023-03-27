with Interfaces;
use  Interfaces;

with GNAT.Byte_Swapping;

package Host_To_Network is
   function Host_To_Network_16 is
      new GNAT.Byte_Swapping.Swapped2(Unsigned_16);

   function Network_To_Host_16 is
      new GNAT.Byte_Swapping.Swapped2(Unsigned_16);

   function Host_To_Network_32 is
      new GNAT.Byte_Swapping.Swapped4(Unsigned_32);

   function Network_To_Host_32 is
      new GNAT.Byte_Swapping.Swapped4(Unsigned_32);


   -- function htons( s : unsigned_short ) return unsigned_short;
   -- pragma import( C, htons );
   --
   -- function ntohs( s : unsigned_short ) return unsigned_short;
   -- pragma import( C, htonl );
   --
   -- function htonl( s : unsigned_long ) return unsigned_long;
   -- pragma import( C, htonl );
   --
   -- function ntohl( s : unsigned_long ) return unsigned_long;
   -- pragma import( C, ntohl );
end Host_To_Network;

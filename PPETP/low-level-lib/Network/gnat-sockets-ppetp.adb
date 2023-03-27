package body GNAT.Sockets.ppetp is
   function Inet_To_Array(Addr: Inet_Addr_Type)
                          return Inet_Buffer
   is
   begin
      return Inet_Buffer(Addr.Sin_V4);
   end Inet_To_Array;


   function To_Int_32(Addr: Inet_Addr_Type) return Unsigned_32 is
      Addr_Array : Inet_Buffer;
   begin

      Addr_Array :=   Inet_To_Array(Addr);

      return      Unsigned_32(addr_array(4)) +
        2 ** 8  * Unsigned_32(addr_array(3)) +
        2 ** 16 * Unsigned_32(addr_array(2)) +
        2 ** 24 * Unsigned_32(addr_array(1));

   end To_Int_32;


end GNAT.Sockets.ppetp;

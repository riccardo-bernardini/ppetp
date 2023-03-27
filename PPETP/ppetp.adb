with Network; 			use Network;
with GNAT.Sockets.ppetp;	use GNAT.Sockets.ppetp;
with Interfaces;		use Interfaces;

package body PPETP is

--     function Hash(Key : Peer_ID) return Hash_Type is
--
--     begin
--    	return Hash_Type (To_Int_32( Key.Addr) + Unsigned_32(Key.Port));
--     end Hash;

--     function Equivalent_Keys(Left, Right : Peer_ID) return boolean is
--     begin
--        return (Left.Addr = Right.Addr) and then (Left.Port = Right.Port);
--     end Equivalent_Keys;


   -- L'indirizzo viene convertito in stringhe, in qeusto modo si garantisce la portabilità
   -- anche con ip_v6... anche se non è molto elegante

   function "=" (Left, Right : Peer_ID) return boolean is
   begin
	return Unsigned_32(Left) = Unsigned_32(Right);
   end "=";


   function "<" (Left, Right : Peer_ID) return boolean is
   begin
	return Unsigned_32(Left) < Unsigned_32(Right);
   end "<";

   function Value(Str: String) return Peer_ID is
      Tmp : Unsigned_32;
   begin

      Tmp := Unsigned_32'Value(Str);
      return Peer_ID(Tmp);

   end Value;

end PPETP;

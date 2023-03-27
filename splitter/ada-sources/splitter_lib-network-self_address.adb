--
with Interfaces.C;                           use Interfaces.C;
with Interfaces.C.Strings;                   use Interfaces.C.Strings;
with Ada.Text_IO;                            use Ada.Text_IO;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Splitter_Lib.BSD_Sockets;               use Splitter_Lib.BSD_Sockets;

package body Splitter_Lib.Network.Self_Address is
   type IP_Config_Access is access IP_Configuration;

   This_Host_IP_Config : IP_Config_Access := null;

   type String_Array is
     array (Natural range <>) of Unbounded_String;

   function Interface_To_IP (If_Name : Chars_Ptr)
                             return Chars_Ptr;
   pragma Import (C, Interface_To_Ip);

   function Interface_Name (Interface_Index : Int)
                           return chars_ptr;
   pragma Import (C, interface_name);

   ------------------
   -- Addr_Inet_Of --
   ------------------

   function Addr_Inet_Of (Net_Interface : String)
      return Inet_Addr_Type
   is
      If_Name : Chars_Ptr := New_String(Net_Interface);
      Result  : Inet_Addr_Type;
   begin
      Result := Inet_Addr(Value(Interface_To_Ip(If_Name)));
      Free (If_Name);
      return Result;
   end Addr_Inet_Of;

   -----------------------
   -- Name_Of_Interface --
   -----------------------

   function Name_Of_Interface (Interface_Index : Natural)
                             return String
   is
      Name : Chars_Ptr := Interface_Name(Int(Interface_Index));
   begin
      if (Name = Null_Ptr) then
         raise Constraint_Error;
      else
         return Value(Name);
      end if;
   end Name_Of_Interface;



   --------------------------
   -- Last_Interface_Index --
   --------------------------

   function Last_Interface_Index
     return Natural
   is
      Result : Natural;
      Name   : Chars_Ptr;
   begin
      Result := 0;
      while (Interface_Name(Int(Result)) /= Null_Ptr) loop
         Result := Result + 1;
      end loop;

      return Result - 1;
   end Last_Interface_Index;

   --------------------------
   -- Available_Interfaces --
   --------------------------

   function Available_Interfaces
     return String_Array
   is
      Result : String_Array (0..Last_Interface_Index);
   begin
      for I in Result'Range loop
         Result (I) := To_Unbounded_String (Name_Of_Interface (I));
      end loop;

      return Result;
   end Available_Interfaces;

   ----------------
   -- My_Address --
   ----------------

   function My_Address
     (Iface : String := "")
      return BSD_Sockets.Inet_Addr_Type
   is
      function To_S(X : Unbounded_String) return String renames To_String;
   begin
      if (Iface = "") then
         declare
            Names : String_Array := Available_Interfaces;
         begin
            for I in Names'Range loop
               if To_S (Names (I)) /= "lo" then
                  return Addr_Inet_Of (To_S (Names (I)));
               end if;
            end loop;
         end;

         return Inet_Addr("127.0.0.1");
      elsif (Iface = "localhost" or Iface = "lo") then
         return Inet_Addr ("127.0.0.1");
      else
         return Addr_Inet_Of (Iface);
      end if;
   end My_Address;

   ----------------------------
   -- Refill_IP_Config_Table --
   ----------------------------

   -- Fill the internal table with the Configuration infos
   procedure Refill_IP_Config_Table is
      Last_Idx : Natural := Last_Interface_Index;
      Names    : String_Array := Available_Interfaces;
      function To_S (X : Unbounded_String) return String renames To_String;

      procedure Free is
        new Ada.Unchecked_Deallocation (IP_Configuration, IP_Config_Access);
   begin
      Free (This_Host_IP_Config);

      This_Host_IP_Config := new IP_Configuration (0 .. Last_Idx);

      for I in Names'Range loop
         This_Host_IP_Config (I) := (Name => Names (I),
                                     Addr => Addr_Inet_Of (To_S (Names (I))));
      end loop;

      This_Host_IP_Config (0) := (Name => To_Unbounded_String("localhost"),
                                  Addr => Inet_Addr("127.0.0.1"));
   end Refill_IP_Config_Table;

begin
   Refill_IP_Config_Table;
end Splitter_Lib.Network.Self_Address;

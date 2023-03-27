
package body Other_Input_Applications is

   Callback_Array : array (Application_Index_Type'Range) of Handler_Processing;


   --------------
   -- Register --
   --------------
   procedure Register(Index    : Application_Index_Type;
                      Callback : Handler_Processing) is
   begin
      if Callback_Array(Index) /= null then
         raise Yet_Registered_Index;
      else
         Callback_Array(Index) := Callback;
      end if;

   end Register;

   ---------------------------
   -- Manage_Unknown_Packet --
   ---------------------------
   function Manage_Unknown_Packet(Data: Byte_Array_Pt) return Boolean is
      Result : Boolean;
   begin
      for i in Callback_Array'First .. Callback_Array'Last loop
         if Callback_Array(i) /= null then
            Result := Callback_Array(i)(Data);
            if Result then
               return True;
            end if;
         end if;
      end loop;

      -- never reach because the Application registered with index 255 accept
      -- all packets and do nothing
      return False;
   end Manage_Unknown_Packet;

end Other_Input_Applications;

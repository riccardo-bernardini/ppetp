package body Profiles.Entangled.Basic is

   function "=" (X, Y: Basic_Ent) return Boolean is
   begin

      if X.Bin_Data'length /= Y.Bin_Data'length then
         return false;
      end if;

      for i in integer range 1.. X.Bin_Data'length loop
         if ( X.Bin_Data(Stream_Element_Offset(i)) /=
               Y.Bin_Data(Stream_Element_Offset(i)) ) then
            return false;
         end if;
      end loop;

      return true;
   end;

end Profiles.Entangled.Basic;

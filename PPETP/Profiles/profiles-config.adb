with Profiles.Config.Basic;
with Profiles.Config.Vandermonde;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package body Profiles.Config is


   -----------------------
   -- New_Config_Parser --
   -----------------------
   function New_Config_Parser (Profile : Profile_Type)
                               return Config_Class_Pt is
      Result : Config_Class_Pt;
   begin
      case Profile is
         when Basic_Profile =>
            Result := Config_Class_Pt(Basic.New_Config_Parser);
         when Vandermonde_Profile =>
            Result := Config_Class_Pt(Vandermonde.New_Config_Parser);
      end case;

      return Result;
   end New_Config_Parser;

   ------------
   -- Insert --
   ------------
   procedure Insert (Table : in out Config_Table;
                     Name  : in     String;
                     Value : in     String) is
      Buffer : Pair_Entry := (Name  => To_Unbounded_String(Name),
                              Value => To_Unbounded_String(Value));
   begin

      -- Find if the element is present
      for I in 1.. Size(Table) loop
         if Get_Name(Table, I ) = Name then
            Table.Data.Replace_Element(I, Buffer);
            return;
         end if;
      end loop;

      Table.Data.Append(Buffer);

   end Insert;

   ------------
   -- Remove --
   ------------
   procedure Remove (Table : in out Config_Table;
                     Name  : in     String) is
   begin
      for I in 1.. Size(Table) loop
         if Get_Name(Table, I ) = Name then
            Table.Data.Delete(I);
            return;
         end if;
      end loop;
   end Remove;

   ----------
   -- Size --
   ----------
   function Size(Table : Config_Table) return Natural
   is
   begin
      return Natural(Table.Data.Length);
   end Size;

   -----------
   -- First --
   -----------
   function First(Table : Config_Table) return Natural is
   begin
      return 1;
   end First;

   ----------
   -- Last --
   ----------
   function Last(Table : Config_Table) return Natural is
   begin
      return Natural (Table.Data.Length);
   end Last;

   --------------
   -- Get_Name --
   --------------
   function Get_Name  (Table : Config_Table;
                       Idx   : Positive) return String is
   begin
      return To_String(Table.Data.Element(Idx).Name);
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------
   function Get_Value (Table : Config_Table;
                       Idx   : Positive) return String is
   begin
      return To_String(Table.Data.Element(Idx).Value);
   end Get_Value;


end Profiles.Config;

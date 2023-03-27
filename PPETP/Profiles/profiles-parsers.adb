with Profiles.Parsers.Basic;
with Profiles.Parsers.Vandermonde;
with Ada.Unchecked_Deallocation;

package body Profiles.Parsers is

   ----------------
   -- New_Parser --
   ----------------

   function New_Parser (Profile : Profile_Type) return Parser_Class_Pt is
      Result : Parser_Class_Pt;
   begin
      case Profile is
         when Basic_Profile =>
            Result := Parser_Class_Pt (Basic.New_Parser);
            when Vandermonde_Profile =>
            return Parser_Class_Pt(Vandermonde.New_Parser);
            -- when Vandermonde =>
            --  return Converter_Class_Pt(Vandermonde.New_Converter);
      end case;



      return Result;
   end New_Parser;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Root_Parser) is
   begin
      null;
   end Finalize;

   ----------------------
   -- New_Parser_Table --
   ----------------------

   function New_Parser_Table return Parser_Table is
      Result : Parser_Table;
   begin
      for I in Result'Range loop
         Result (I) := New_Parser (I);
      end loop;

      return Result;
   end New_Parser_Table;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Parser_Table) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Object => Root_Parser'Class,
         Name => Parser_Class_Pt);
   begin
      for I in Profile_Type loop
         Finalize (Object (I).all);
         Free (Object (I));
      end loop;
   end Finalize;

end Profiles.Parsers;

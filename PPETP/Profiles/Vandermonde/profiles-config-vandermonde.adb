with Ada.Characters.Handling;			use Ada.Characters.Handling;

with Profiles.Parameters.Vandermonde;		use Profiles.Parameters.Vandermonde;
with Galois_Field;				use Galois_Field;

--with Ada.Text_IO;		use Ada.Text_IO;
package body Profiles.Config.Vandermonde is

   -----------------------
   -- New_Config_Parser --
   -----------------------
   function New_Config_Parser return Config_Parser_Pt is
   begin
      return new Config_Parser;
   end New_Config_Parser;

   -----------
   -- Parse --
   -----------
   procedure  Parse (Parser : in out Config_Parser;
                     Table  : in     Config_Table;
                     Result :    out Parameters_Class_Pt;
                     Errors : in out Config_Error_List) is

      RF : Unbounded_String := To_Unbounded_String("");
      GE : Unbounded_String := To_Unbounded_String("");
      GF : Unbounded_String := To_Unbounded_String("");
      RF_Valid : Boolean := False;
      GE_Valid : Boolean := False;
      GF_Valid : Boolean := False;

      Error : Boolean := False;
   begin

--      Put_Line(Size(Table)'img);
      for I in 1 .. Size(Table) loop

--         Put_Line(To_Upper(Get_Name(Table, I)));
         case Valid_Vandermonde_Parameters'Value(To_Upper(Get_Name(Table, I))) is
            when REDUCTION_FACTOR =>
--               Put_Line("RF :" & Get_Value(Table, I));
               RF := To_Unbounded_String(Get_Value(Table, I));
               RF_Valid := True;
            when GALOIS_ELEMENT =>
--               Put_Line("GE :" & Get_Value(Table, I));
               GE := To_Unbounded_String(Get_Value(Table, I));
               GE_Valid := True;
            when GF_SIZE =>
--               Put_Line("GF :" & Get_Value(Table, I));
               GF := To_Unbounded_String(Get_Value(Table, I));
               GF_valid := True;
            when others =>
               -- Unknown Parameter
               Errors.Append ((Name   => To_Unbounded_String(Get_Name(Table, I)),
                               Value  => To_Unbounded_String(Get_Value(Table, I)),
                               Reason => Param_Unknown));
         end case;

      end loop;


      -- Invalid_Value
      declare
         Red  : Max_Range;
         Gal  : Max_Range := 0;  -- default value;
         Size : Max_Range;
      begin


         -- Numeric Conversion
         if RF_Valid then
            begin
               Red := Max_Range'Value(To_String(RF));
            exception
               when e: others =>
                  Errors.Append ((Name   => To_Unbounded_String("REDUCTION_FACTOR"),
                                  Value  => RF,
                                  Reason => Invalid_Value));
                  Error := True;
            end;
         end if;

         -- Numeric Conversion
         if GE_Valid then
            begin
               Gal := Max_Range'Value(To_String(GE));
            exception
               when e: others =>
                  Errors.Append ((Name   => To_Unbounded_String("GALOIS_ELEMENT"),
                                  Value  => GE,
                                  Reason => Invalid_Value));
                  Error := True;
            end;
         end if;

         -- Numeric Conversion
         if GF_Valid then
            begin
               Size := Max_Range'Value(To_String(GF));
            exception
               when e: others =>
                  Errors.Append ((Name   => To_Unbounded_String("GF_SIZE"),
                                  Value  => GF,
                                  Reason => Invalid_Value));
                  Error := True;
            end;
         end if;

         -- Galois element in Galois Field range
         if GF_Valid and GE_Valid then
            if Gal not in  0..(2**Integer(Size))-1 then
               Errors.Append ((Name   => To_Unbounded_String("GALOIS_ELEMENT"),
                               Value  => GE,
                               Reason => Invalid_Value));
               Error := True;
            end if;
         end if;

         if Error then
            Result := null;
         else
            Result := new Vandermonde_Par'(Reduction_Factor => Natural(Red),  --> è R
                                           RF_Valid         => RF_Valid,
                                           Galois_Element   => To_Galois(Integer(Gal)),   --> è b!
                                           GE_Valid         => GE_Valid,
                                           GF_Size          => Natural(Size) ,
                                           GF_Valid         => GF_Valid);
         end if;

      end;

   end Parse;

end Profiles.Config.Vandermonde;

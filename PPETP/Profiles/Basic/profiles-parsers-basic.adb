with Profiles.Parameters.Basic;
with Byte_Arrays;        use Byte_Arrays;

package body Profiles.Parsers.Basic is
   -- ================ --
   -- ==== PARSER ==== --
   -- ================ --


   ----------------
   -- New_Parser --
   ----------------

   function New_Parser return Parser_Pt is
   begin
      return new Parser;
   end New_Parser;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (Handler : in out Parser;
                          Default : in     Parameters_Class_Pt)
   is
   begin
      null;
   end Set_Default;

   ----------------
   -- Parse_Data --
   ----------------

   procedure Parse_Data
     (Handler   : in     Parser;
      Flags     : in     Profile_Flags;
      Inline    : in     Boolean;
      Source    : in out Parsing_Buffer;
      Result    :    out Entangled_Payload_Pt)
   is
      procedure Get is
        new Get_Remaining (Target => byte_array_Pt);

      Buf : Byte_Array_Pt;
   begin
      Get(Source, Buf);
      Result := Entangled_Payload_Pt'
        (new Basic_Ent'(Bin_Data   => Buf));
   end Parse_Data;

   ----------------------
   -- Parse_Parameters --
   ----------------------

   procedure Parse_Parameters
     (Handler    : in     Parser;
      Source     : in out Parsing_Buffer;
      Parameters :    out Parameters_Class_Pt)
   is
   begin
      Parameters := Profiles.Parameters.Basic.New_Parameter;
   end Parse_Parameters;


end Profiles.Parsers.Basic;

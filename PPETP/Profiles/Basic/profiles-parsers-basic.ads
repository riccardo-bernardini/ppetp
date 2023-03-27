with Parsing_Buffers;           use Parsing_Buffers;
with profiles.entangled.Basic;  use profiles.entangled.Basic;

package  profiles.parsers.basic is
   -- ============= --
   -- = Converter = --
   -- ============= --

   type Parser is new Root_Parser with private;
   type Parser_Pt is access Parser;

   function New_Parser return Parser_Pt;

   procedure Set_Default (Handler : in out Parser;
                          Default : in     Parameters_Class_Pt);

   procedure Parse_Data (Handler   : in     Parser;
                         Flags     : in     Profile_Flags;
                         Inline    : in     Boolean;
                         Source    : in out Parsing_Buffer;
                         Result    :    out Entangled_Payload_Pt);

   -- This procedure takes as input (i) the  flags (Profile flags
   -- and Inline) found in the data packet (ii) the
   -- Parsing_Buffer From pointing just after the fixed
   -- header section and (iii) the default Profile parameters.
   -- It returns a pointer to a structure
   -- describing the profile data.

   procedure Parse_Parameters (Handler : in     Parser;
                               Source  : in out Parsing_Buffer;
                               Parameters :    out Parameters_Class_Pt);
   -- This procedure is called to parse the detail payload
   -- of a Set_Default command.  Source points to the beginning
   -- of the payload section.
private
   type Parser       is new Root_Parser       with null record;
end profiles.parsers.basic;

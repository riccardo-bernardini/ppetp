with Profiles.Parameters;    use Profiles.Parameters;
with Profiles.Entangled;     use Profiles.Entangled;
with Parsing_Buffers;        use Parsing_Buffers;

with Ada.Unchecked_Deallocation;

package Profiles.Parsers is
   -- ============ --
   -- == Parser == --
   -- ============ --

   type Root_Parser is abstract new Root_Profile_Handler with private;

   type Parser_Class_Pt is access all Root_Parser'Class;

   type Parser_Table is array (Profile_Type) of Parser_Class_Pt;


   Uncompatible_Parameters : exception;

   function New_Parser (Profile : Profile_Type) return Parser_Class_Pt;

   procedure Finalize (Object : in out Root_Parser);

   function New_Parser_Table return Parser_Table;

   procedure Finalize (Object : in out Parser_Table);

   procedure Set_Default
     (Handler : in out Root_Parser;
      Default : in Parameters_Class_Pt)
   is abstract;

   procedure Parse_Data
     (Handler : in Root_Parser;
      Flags   : in Profile_Flags;
      Inline  : in Boolean;
      Source  : in out Parsing_Buffer;
      Result  : out Entangled_Payload_Pt)
   is abstract;
   -- This procedure takes as input (i) the  flags (Profile flags
   -- and Inline) found in the data packet (ii) the
   -- Parsing_Buffer From pointing just after the fixed
   -- header section and (iii) the default Profile parameters.
   -- It returns a pointer to a structure
   -- describing the profile data.

   procedure Parse_Parameters
     (Handler    : in Root_Parser;
      Source     : in out Parsing_Buffer;
      Parameters : out Parameters_Class_Pt)
   is abstract;
   -- This procedure is called to parse the detail payload
   -- of a Set_Default command.  Source points to the beginning
   -- of the payload section.
private
   type Root_Parser is abstract
     new Root_Profile_Handler with null record;
end Profiles.Parsers;

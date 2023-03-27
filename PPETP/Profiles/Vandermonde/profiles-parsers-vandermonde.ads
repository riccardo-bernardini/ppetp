with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;

with Parsing_Buffers;                  use Parsing_Buffers;
with Galois_Matrices;                  use Galois_Matrices;
with Galois_Field;                     use Galois_Field;

with Ada.Unchecked_Deallocation;

package  profiles.parsers.vandermonde is
   -- ============= --
   -- = Converter = --
   -- ============= --
   -- Questo tipo di oggetto deve fare l'ANALISI del pacchetto che arriva dalla rete.
   -- Prende tutti i dati dal livello trasporto e li processa a livello Profile.
   -- Mi attendo in ingresso un pacchetto composto da un'Array di Bytes e darò in uscita
   -- un entangled_data e setterò i parametri di Parameters.

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

   procedure Free (Object: in out Parser_Pt);

private
   type Parser       is new Root_Parser       with
      record
         --         Parser_Param : Parameters_Class_Pt;
         Parser_Param : Vandermonde_Par;
      end record;

end profiles.parsers.vandermonde;

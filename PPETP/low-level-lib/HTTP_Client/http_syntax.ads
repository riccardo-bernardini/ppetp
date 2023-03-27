with GNAT.Spitbol.Patterns;           use GNAT.Spitbol, GNAT.Spitbol.Patterns;
with Ada.Strings.Maps.Constants;      use Ada.Strings,
    Ada.Strings.Maps.Constants, Ada.Strings.Maps;
with Ada.Characters.Latin_1;          use Ada.Characters;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
--
-- This package contains few constants and procedure useful to parse
-- HTTP-related stuff.
--
-- Status: <FULLY TESTED>
--
package HTTP_Syntax is
   pragma Elaborate_Body;

   Octet_set : constant Maps.Character_Set :=
                 To_Set (Character_Range'(Low  => Character'Val (0),
                                          High => Character'Val (255)));

   Text_Set  : constant Maps.Character_Set :=
                 (Octet_Set and not Control_Set);

   Separator_Set : constant Maps.Character_Set :=
                     To_Set ("()<>@,;:/\[]?={}""" & Latin_1.SPA & Latin_1.HT);

   Token_Set  : constant Character_Set :=
                  Iso_646_Set and not (Control_Set or Separator_Set);

   CText_Set  : constant Character_Set := Text_Set and not To_Set ("()");
   QText_Set  : constant Character_Set := Text_Set and not To_Set ('"');

   Octet      : constant Pattern := Any (Octet_Set);
   Char       : constant Pattern := Any (Iso_646_Set);
   Up_Alpha   : constant Pattern := Any (Upper_Set and Iso_646_Set);
   Lo_Alpha   : constant Pattern := Any (Lower_Set and Iso_646_Set);
   Alpha      : constant Pattern := Any (Letter_Set and Iso_646_Set);
   Digit      : constant Pattern := Any (Decimal_Digit_Set);
   HEX        : constant Pattern := Any (Hexadecimal_Digit_Set);

   Ctl        : constant Pattern := Any (Control_Set and Iso_646_Set);
   CR         : constant Pattern := Any (Latin_1.CR);
   LF         : constant Pattern := Any (Latin_1.LF);
   SP         : constant Pattern := Any (Latin_1.SPA);
   HT         : constant Pattern := Any (Latin_1.HT);

   CRLF       : constant Pattern := CR & LF;
   LWS        : constant Pattern :=
                 (CRLF or Succeed) & (SP or HT) & Arbno (SP or HT);

   Separator  : constant Pattern := Any (Separator_Set);

   TEXT       : constant Pattern := Any (Text_Set) or LWS;
   CText      : constant Pattern := Any (Ctext_Set);
   QText      : constant Pattern := Any (Qtext_Set);

   Token_Char : constant Pattern := Any (Token_Set);
   Token      : constant Pattern := Span(Token_Set);

   Quoted_Pair   : constant Pattern := "\" & Char;
   Quoted_String : constant Pattern :=
                     """" & Arbno (Qtext or Quoted_Pair) & """";

   HTTP_Integer     : constant Pattern :=  Span (Decimal_Digit_Set);
   -- Comment is a recursive pattern. It must be initialized in
   -- the body.
   Comment            : Pattern;

   HTTP_Version       : constant Pattern :=
                        "HTTP/" & HTTP_Integer & "." & HTTP_Integer;

   Wk_Day             : constant Pattern :=
                        "Mon" or "Tue" or "Wed" or "Thu" or
                  "Fri" or "Sat" or "Sun";

   Charset            : constant Pattern := Token;
   Attribute_Value    : constant Pattern := Token or Quoted_String;
   Parameter          : constant Pattern := Token & "=" & Attribute_Value;
   Content_Encoding   : constant Pattern := Token;
   Parameter_List     : constant Pattern := Arbno (";" & Parameter);
   Transfer_Extension : constant Pattern := Token & Parameter_List;

   Media_Type         : constant Pattern :=
                          Token & "/" & Token & Parameter_List;


   type HTTP_Code is new Integer range 100 .. 999;

   type HTTP_Code_Category is (Informational, Success, Redirection,
                               Client_Error,  Server_Error, Extension);

   type HTTP_Status is
      record
         Code       : HTTP_Code;
         Message    : Unbounded_String;
         Category   : HTTP_Code_Category;
      end record;

   function Parse_HTTP_Status (Status_Line : String) return HTTP_Status;

   procedure Parse_HTTP_Header (Line           : in     Unbounded_String;
                                Header_Name    :    out Unbounded_String;
                                Header_Value   :    out Unbounded_String;
                                Name_Lowercase : in     Boolean := True);

   procedure Parse_Chunk_Size (Line      : in     Unbounded_String;
                               Size      :    out Natural;
                               Extension :    out Unbounded_String);

   Malformed_Status_Line : exception;
   Malformed_Header      : exception;
end HTTP_Syntax;

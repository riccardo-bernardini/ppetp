--                              -*- Mode: Ada -*-
--  Filename        : configuration-xml.ads
--  Description     : Function to parse configuration description in XML format
--  Author          : Riccardo Bernardini
--  Created On      : Mon Nov 10 11:06:08 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- The current PPETP specs describe an XML format which can be used to
-- describe a session configuration.  The function provided by this
-- package takes such a description and convert it to an array of
-- Session_Config's (defined in configuration.ads).
--

with Body_Grammar;
pragma Elaborate_All (Body_Grammar);

package Configuration.Xml is
   Parse_Error : exception;

   function Parse (Str : String) return Config_Data;
end Configuration.Xml;

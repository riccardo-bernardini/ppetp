--                              -*- Mode: Ada -*-
--  Filename        : url.ads
--  Description     : URL parsing functions
--  Author          : Riccardo Bernardini
--  Created On      : Wed Feb 27 17:39:30 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

--
-- This package provides a function Parse which decompose an URL
-- (http-like) into its basic components.  The URL structure is
--
--      <scheme>://<user>:<password>@<host>:<port>/<url-path>
--
-- Some or all of the parts "<user>:<password>@", ":<password>",
-- ":<port>", and "/<url-path>" may be missing.
--
-- In order to parse an URL the user can call function
-- Parse which returns a record of type URL_Info.  If the string
-- given to Parse is not a valid URL, Parse return No_URL.
--

with Ada.Strings.Unbounded;   use  Ada.Strings.Unbounded;
with GNAT.Sockets;            use GNAT.Sockets;

package URL is
   --
   -- Records of type URL_Info represent the component of an URL.  The
   -- field name should be auto-explicative.  If some part is missing
   -- from the URL, the corresponding field is equal to
   -- Null_Unbounded_String.  The only exception to this rule is the field
   -- Port which is equal to No_Port if the ":<port>" part is missing
   -- from the URL
   --
   -- URL used in the example:
   --   ftp://login:password@www.yahoo.com:8080/path/to/file.txt
   --
   type URL_Info is
      record
         Scheme   : Unbounded_String;  -- e.g. "ftp"
         User     : Unbounded_String;  -- e.g. "login"
         Password : Unbounded_String;  -- e.g. "password"
         Host     : Unbounded_String;  -- e.g. "www.yahoo.com"
         Port     : Port_Type;         -- e.g. 8080
         Path     : Unbounded_String;  -- e.g. "path/to/file.txt"
      end record;

   No_URL : constant URL_Info;

   --
   -- Parse URL X and returns a record of URL_Info with the
   -- components of X.  If X is not a valid URL, return No_URL.
   --
   function Parse(X : String) return URL_Info;
   function Parse(X : Unbounded_String) return URL_Info;

   --
   -- Print to the standard output a representation of X. Useful
   -- mainly for debug.
   --
   procedure Print(X : URL_Info);
private
   No_URL : constant URL_Info :=
     (Scheme   => Null_Unbounded_String,
      User     => Null_Unbounded_String,
      Password => Null_Unbounded_String,
      Host     => Null_Unbounded_String,
      Port     => No_Port,
      Path     => Null_Unbounded_String);
end URL;

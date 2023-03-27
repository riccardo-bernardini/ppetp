with GNAT.Spitbol.Patterns;
use GNAT.Spitbol.Patterns;
use GNAT.Spitbol;

with Text_Io; use Text_Io;

package body URL is
   URL_Char : constant String :=
     "abcdefghijklmnopqrstuvwxyz" &
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
     "0123456789" &
     "$-_.+!*'(),";

   Scheme_Value   : aliased Vstring;
   User_Value     : aliased Vstring;
   Password_Value : aliased Vstring;
   Host_Value     : aliased Vstring;
   Port_Value     : aliased Vstring;
   Path_Value     : aliased Vstring;

   Scheme     : constant Pattern := Span(URL_Char);
   User       : constant Pattern := Span(URL_Char);
   Password   : constant Pattern := Span(URL_Char);
   Host       : constant Pattern := Span(URL_Char);
   Port       : constant Pattern := Span("0123456789");
   Path       : constant Pattern := Span(URL_Char & "/");

   Scheme_Part : constant Pattern := (Scheme ** Scheme_Value & "://");

   User_Part   : constant Pattern :=
     User ** User_Value & ((":" & Password ** Password_Value) or "") &  "@";

   Host_Part   : constant Pattern := Host ** Host_Value;

   Port_Part   : constant Pattern := ":" & Port ** Port_Value;

   Path_Part   : constant Pattern := "/" & Path ** Path_Value;

   URL_Pattern : constant Pattern :=
     Pos(0)              &
     (Scheme_Part or "") &
     (User_Part   or "") &
     (Host_Part)         &
     (Port_Part   or "") &
     (Path_Part   or "") &
     Rpos(0);

   -----------
   -- Parse --
   -----------

   function Parse (X : String) return URL_Info is
      Result : URL_Info;
      Port   : Port_Type;

      function U(X : Vstring) return Unbounded_String is
      begin
         return To_Unbounded_String(S(X));
      end U;
   begin
      Scheme_Value   := V("");
      User_Value     := V("");
      Password_Value := V("");
      Host_Value     := V("");
      Port_Value     := V("");
      Path_Value     := V("");

      if Match(X, URL_Pattern) then
         -- Put_Line("'" & X & "' -> OK");

         if (S(Port_Value) = "") then
            Port := No_Port;
         else
            Port := Port_Type(Integer'Value(S(Port_Value)));
         end if;

         Result := (Scheme   => U(Scheme_Value),
                    User     => U(User_Value),
                    Password => U(Password_Value),
                    Host     => U(Host_Value),
                    Port     => Port,
                    Path     => U(Path_Value));
      else
         -- Put_Line("'" & X & "' -> BAD");
         Result := No_URL;
      end if;

      return Result;
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse (X : Unbounded_String) return URL_Info is
   begin
      return Parse (To_String(X));
   end Parse;

   procedure Print(X : URL_Info) is
   begin
      if (X = No_URL) then
         Put_Line ("Empty URL");
      else
         put_line("Scheme   => [" & To_String(X.Scheme)     & "]");
         put_line("User     => [" & To_String(X.User)       & "]");
         put_line("Password => [" & To_String(X.Password)   & "]");
         put_line("Host     => [" & To_String(X.Host)       & "]");
         put_line("Port     => [" & Port_Type'Image(X.Port) & "]");
         put_line("Path     => [" & To_String(X.Path)       & "]");
      end if;
   end Print;
end URL;


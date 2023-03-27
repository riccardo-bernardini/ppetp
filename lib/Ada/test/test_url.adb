with Url;
with Ada.Command_Line;

with Text_Io;
use  Text_Io;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;            use GNAT.Sockets;

procedure Test_Url is
   function U(X: String) return Unbounded_String renames To_Unbounded_String;

   type Test_Entry is
      record
         Input  : Unbounded_String;
         Result : URL.URL_Info;
      end record;

   type Test_Cases is array(Positive range <>) of Test_Entry;

   Empty : constant Unbounded_String := U("");

   Cases : Test_Cases :=
     (1 => (Input  => U("http://www.yahoo.com/index.html"),
            Result => (Scheme   => U("http"),
                       User     => Empty,
                       Password => Empty,
                       Host     => U("www.yahoo.com"),
                       Port     => No_Port,
                       Path     => U("index.html"))),
      2 => (Input  => U("www.yahoo.com"),
            Result => (Scheme   => Empty,
                       User     => Empty,
                       Password => Empty,
                       Host     => U("www.yahoo.com"),
                       Port     => No_Port,
                       Path     => Empty)),
      3 => (Input  => U("ftp://pippo:pass@www.yahoo.com"),
            Result => (Scheme   => U("ftp"),
                       User     => U("pippo"),
                       Password => U("pass"),
                       Host     => U("www.yahoo.com"),
                       Port     => No_Port,
                       Path     => Empty)),
      4 => (Input  => U("ww:w.yahoo.com"),
            Result => URL.No_URL));

   X  : URL.Url_Info;
   Ok : Boolean;

   Done   : Natural;
   Passed : Natural;

   use type URL.Url_Info;
begin
   Ok := True;
   Done := 0;
   Passed := 0;
   for I in Cases'Range loop
      --      Put_Line("Test N. " & Integer'Image(I) & "["
      --               & To_String(Cases(I).Input) &"]");

      X := URL.Parse(Cases(I).Input);
      if (X = Cases(I).Result) then
         Passed := Passed + 1;
      else
         Put_Line("Test " & Integer'Image(I) & " failed");
         Put("Expected: ");
         URL.Print(Cases(I).Result);
         Put("Obtained: ");
         URL.Print(X);

         Ok := False;
      end if;

      Done := Done + 1;
   end loop;

   if (Ok) then
      Put("SUCCESS ");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
   else
      Put("FAILURE ");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;

   Put_Line ("passed "
             & Integer'Image(Passed)
             & " tests out of "
             & Integer'Image(Done));
end Test_Url;

-- Old stuff
-- if (Argument_Count < 1) then
--    Put_Line("Usage: test_url <URL>");
--    Set_Exit_Status(Failure);
--    return;
-- end if;
--
-- URL.Print(URL.Parse(Argument(1)));
--
-- Set_Exit_Status(Success);

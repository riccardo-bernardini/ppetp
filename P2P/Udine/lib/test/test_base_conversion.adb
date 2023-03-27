with Base_Conversion;

with Test_Report;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Text_Io;
use  Text_Io;

procedure Test_Base_Conversion is
   function U (Source : in String)
              return Unbounded_String
     renames To_Unbounded_String;

   package C is
      new Base_Conversion(Integer);

   type Test_Case is
      record
         Numero : Integer;
         Testo  : Unbounded_String;
         Zero_Pad : Boolean := True;
         Base   : Integer := 16;
      end record;

   type Many_Cases is array (Positive range <>) of Test_Case;

   Tests : Many_Cases := ((Numero   => 42,
                           Testo    => U("00002A"),
                           Zero_Pad => True,
                           Base     => 16),
                          (Numero   => 42,
                           Testo    => U("    2A"),
                           Base     => 16,
                           Zero_Pad => False),
                          (Numero   => 42,
                           Testo    => U("101010"),
                           Base     => 2,
                           Zero_Pad => True));


   Reporter : Test_Report.Reporter_Type;

   function Check (This_Case : Test_Case)
                  return Boolean is
      Buffer : String(1..6);
   begin
      C.To_String(Item     => This_Case.Numero,
                  To       => Buffer,
                  Base     => This_Case.Base,
                  Zero_Pad => This_Case.Zero_Pad);

      Put_Line("[" & Buffer & "] [" & To_String(This_Case.Testo) & "]");

      return This_Case.Testo = U(Buffer);
   end Check;

   procedure Suite_1 is
      new Test_Report.Do_Suite (Test_Case       => Test_Case,
                                Test_Case_Array => Many_Cases,
                                Check           => Check);
begin
   Suite_1 (Reporter, Tests, "Prima");

   Test_Report.Final(Reporter);
end Test_Base_Conversion;

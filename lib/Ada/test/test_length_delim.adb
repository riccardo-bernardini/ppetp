with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Length_Delimited_Strings;
use  Length_Delimited_Strings;

with Text_Io;
use  Text_Io;

procedure Test_Length_Delim is
   function Unb(X : String) return Unbounded_String
     renames To_Unbounded_String;

   type Str_Array is array(Positive range <>) of Unbounded_String;

   type Test_Case is record
      Input  : Unbounded_String;
      Output : Str_Array(1..10);
      N_El   : Natural;
      Ok     : Boolean;
   end record;

   type Test_Case_Array is array(Positive range <>) of Test_Case;

   Cases : Test_Case_Array :=
     (1 => (Input  => Unb("xx 3:poi +/6:nasone  v"),
            Output => (1 => Unb("poi"),
                       2 => Unb("nasone"),
                       others => Null_Unbounded_String),
            N_El   => 2,
            Ok     => True),

      2 => (Input  => Unb("##4:poi"),
            Output => (others => Null_Unbounded_String),
            N_El   => 0,
            Ok     => False),

      3 => (Input  => Unb("13:cipolla lessa"),
            Output => (1 => Unb("cipolla lessa"),
                       others => Null_Unbounded_String),
            N_El   => 1,
            Ok     => True));

   Set : LDS_Set_Type;
   Ok  : Boolean;
begin
   for I in Cases'Range loop
      Ok := True;
      begin
         Load(Set, To_String(Cases(I).Input));
      exception
         when Malformed_LDS =>
            Ok := False;
      end;

      if (not Ok and Cases(I).Ok) then
         Put_Line("Test #"
                  & Integer'Image(I)
                  & ": unexpected failure");
      elsif (Ok and not Cases(I).Ok) then
         Put_Line("Test #"
                  & Integer'Image(I)
                  & ": unexpected un-failure");
      elsif (not Ok and not Cases(I).Ok) then
         Put_Line("Test #" & Integer'Image(I)
                  & ": OK [failed as expected (that's ok)]");
      elsif (Ok and Cases(I).Ok) then
         if (N_Elem(Set) /= Cases(I).N_El) then
            Ok := False;
         else
            for K in 1..Cases(I).N_El loop
               if (To_String(Cases(I).Output(K)) /=  Element(Set, K)) then
                  Ok := False;
                  exit;
               end if;
            end loop;
         end if;

         if (Ok) then
            Put_Line("Test #" & Integer'Image(I) & ": OK");
         else
            Put_Line("Test #"
                     & Integer'Image(I)
                     & " failed");

            Put_Line ("Expected: Size=" & Integer'Image(Cases(I).N_El));
            for K in 1..Cases(I).N_El loop
               Put_Line ("'" & To_String(Cases(I).Output(K)) & "'");
            end loop;

            Put_Line ("Got: Size=" & Integer'Image(N_Elem(Set)));
            for K in 1..N_Elem(Set) loop
               Put_Line ("'" & Element(Set, K) & "'");
            end loop;
         end if;
      else
         null;
      end if;
   end loop;
end Test_Length_Delim;

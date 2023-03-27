with Test_Report;      use Test_Report;
with Str_Conversion;   use Str_Conversion;
with Text_Io;          use Text_Io;

procedure Test_Conversion is
   type Mod256 is mod 256;
   type Mod_Array is array (Positive range <>) of Mod256;
   type Int_array is array (Positive range <>) of Integer;
   type String_Pt is access String;

   type Example is
      record
         Str : String_Pt;
         Val : Integer;
      end record;

   type Example_Array is array (Positive range <>) of Example;

   Test_Cases : Example_Array :=
     (1 => (Str => new String'("123"),  Val => 123),
      2 => (Str => new String'("0"),    Val => 0),
      3 => (Str => new String'("-15"),  Val => -15),
      4 => (Str => new String'(" -42"), Val => -42),
      5 => (Str => new String'(" 24"),  Val => 24));

   Int_Cases : Int_Array :=
     (1, 0, -12, 24, 532, -1230, 1024, -1024);

   Mod_Cases : Mod_Array :=
     (2, 4, 8, 10, 255, 0);

   function A_To_I is
      new String_To_Integer (Integer);

   function I_To_A is
      new Integer_To_String (Integer);

   function I_To_A is
      new Modular_To_String (Mod256);

   function Check_A_To_I (This : Example) return Boolean is
   begin
      return A_To_I(This.Str.all, 10) = This.Val;
   end Check_A_To_I;

   function Check_Two_Way (This : Integer) return Boolean is
      Ok : Boolean := True;
   begin
      for Base in Base_Type loop
         -- Tmp := A_To_I(I_To_A(This, Base), Base);
         -- Put_Line (Integer'Image(This) & "  " &
         --           Integer'Image(Base) & " -> " &
         --           Integer'Image(Tmp)  &  " '" &
         --           I_To_A(This, Base)  &  "'  " &
         --           Boolean'Image(This = Tmp));

         if This  /= A_To_I(I_To_A(This, Base), Base) then
            Ok := False;
         end if;
      end loop;

      return Ok;
   end Check_Two_Way;

   function Check_Two_Way (This : Mod256) return Boolean is

   begin
      for Base in Base_Type loop
         if This  /= Mod256(A_To_I(I_To_A(This, Base), Base)) then
            return False;
         end if;
      end loop;

      return True;
   end Check_Two_Way;

   procedure Do_A_To_I is
      new Do_Suite (Test_Case       => Example,
                    Test_Case_Array => Example_Array,
                    Check           => Check_A_To_I);

   procedure Do_2way_Int is
      new Do_Suite (Test_Case       => Integer,
                    Test_Case_Array => Int_Array,
                    Check           => Check_Two_Way);

   procedure Do_2way_Mod is
      new Do_Suite (Test_Case       => Mod256,
                    Test_Case_Array => Mod_Array,
                    Check           => Check_Two_Way);

   Reporter : Reporter_Type;
begin
   -- Reporter.Be_Verbose;
   Do_A_To_I   (Reporter, Test_Cases, "string -> integer");
   Do_2way_Mod (Reporter, Mod_Cases,  "2 way 'mod'");
   Do_2way_Int (Reporter, Int_Cases,  "2 way 'integer'");
   Final(Reporter);
end Test_Conversion;

with Network;             use Network;
with Network_Utilities;   use Network_Utilities;
with Test_Report;         use Test_Report;
with Text_Io;             use Text_Io;

procedure Test_Net_Utilities is
   subtype Inet_Buf is  Inet_Addr_VN_Buffer;
   type Inet_Addr_Buf_Pt is access Inet_Addr_VN_Buffer;
   type String_Pt is access String;

   type Dot_Notation_Example is
      record
         Str : String_Pt;
         Buf : Inet_Addr_Buf_Pt;
      end record;

   type Dot_Notation_Array is array (Positive range <>)
     of Dot_Notation_Example;

   Dot_Notation_Cases : Dot_Notation_Array :=
     (1 => (Str => new String'("115.123.48.1"),
            Buf => new Inet_Buf'(115, 123, 48, 1)),
      2 => (Str => new String'("0.0.255.255"),
            Buf => new Inet_Buf'(0, 0, 255, 255)),
      3 => (Str => new String'("0:0:0:0:0:0:0:0"),
            Buf => new Inet_Buf'(1..16 => 0)),
      4 => (Str => new String'("1:203:405:607:809:a0b:c0d:e0f"),
            Buf => new Inet_Buf'(0,  1,  2,  3,
                                 4,  5,  6,  7,
                                 8,  9,  10, 11,
                                 12, 13, 14, 15)),
      5 => (Str => new String'("fffe:fdfc:fbfa:f9f8:f7f6:f5f4:f3f2:f1f0"),
            Buf => new Inet_Buf'(255, 254, 253, 252,
                                 251, 250, 249, 248,
                                 247, 246, 245, 244,
                                 243, 242, 241, 240))
      );

   function Test_Dot_Notation (Exmp : Dot_Notation_Example)
                              return Boolean
   is
   begin
      -- Put_line("[" & To_Dot_Notation(Exmp.Buf.all) & "]");
      return To_Dot_Notation(Exmp.Buf.all) = Exmp.Str.all;
   end Test_Dot_Notation;

   function Test_Conversion (Exmp : Dot_Notation_Example)
                            return Boolean
   is
      IPV6_Unimplemented : constant Boolean := True;
   begin
      if (Exmp.Buf'Length > 4 and IPV6_Unimplemented) then
         return True;
      else
         return Inet_addr(Exmp.Buf.all) = Inet_addr(Exmp.Str.all);
      end if;
   end Test_Conversion;

   function Test_Two_Way  (Exmp : Dot_Notation_Example)
                            return Boolean
   is
      IPV6_Unimplemented : constant Boolean := True;
   begin
      if (Exmp.Buf'Length > 4 and IPV6_Unimplemented) then
         return True;
      else
         return Exmp.Buf.all = To_Array(Inet_Addr(Exmp.Buf.all));
      end if;
   end Test_Two_Way;

   procedure Dot_Notation_Suite is
      new Do_Suite (Test_Case       => Dot_Notation_Example,
                    Test_Case_Array => Dot_Notation_Array,
                    Check           => Test_Dot_Notation);

   procedure Conversion_Suite is
      new Do_Suite (Test_Case       => Dot_Notation_Example,
                    Test_Case_Array => Dot_Notation_Array,
                    Check           => Test_Conversion);

   procedure Two_Way_Suite is
      new Do_Suite (Test_Case       => Dot_Notation_Example,
                    Test_Case_Array => Dot_Notation_Array,
                    Check           => Test_Two_Way);

   Reporter : Reporter_Type;
begin
   -- Reporter.Be_Verbose;
   Reporter.Set_Tab(28);
   Dot_Notation_Suite (Reporter, Dot_Notation_Cases, "Dot Notation");
   Conversion_Suite   (Reporter, Dot_Notation_Cases, "Conversion");
   Two_Way_Suite      (Reporter, Dot_Notation_Cases, "array <-> inet");
   Reporter.Final;
end Test_Net_Utilities;

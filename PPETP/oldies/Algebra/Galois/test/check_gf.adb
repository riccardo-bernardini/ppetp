with GF_Generator;
with Check_Field;
with GF8, GF16, GF32;

with Test_Status;
use  Test_Status;

procedure Check_Gf is
   package GF8_Generator is new GF_Generator(GF8, 8);
   subtype GF8_Gen is GF8_Generator.Generator;
   use GF8_Generator;
   use type GF8.Galois;

   package Check_Gf8 is
      new Check_field(Field     => GF8.Galois,
                      Zero      => GF8.Zero,
                      One       => GF8.One,
                      Name      => "GF(2^8)",
                      Inv       => GF8.Inv,
                      Image     => GF8.Image,
                      Generator => GF8_Gen);

   package GF16_Generator is new GF_Generator(GF16, 8);
   subtype GF16_Gen is GF16_Generator.Generator;
   use Gf16_Generator;
   use type GF16.Galois;

   package Check_Gf16 is
      new Check_field(Field     => GF16.Galois,
                      Zero      => GF16.Zero,
                      One       => GF16.One,
                      Name      => "GF(2^16)",
                      Inv       => GF16.Inv,
                      Image     => GF16.Image,
                      Generator => GF16_Gen);

   package GF32_Generator is new GF_Generator(GF32, 8);
   subtype GF32_Gen is GF32_Generator.Generator;
   use GF32_Generator;
   use type GF32.Galois;

   package Check_Gf32 is
      new Check_field(Field     => GF32.Galois,
                      Zero      => GF32.Zero,
                      One       => GF32.One,
                      Name      => "GF(2^32)",
                      Inv       => GF32.Inv,
                      Image     => GF32.Image,
                      Generator => GF32_Gen);

   Status : Status_Type;
begin
   Status.New_Test;
   Status.Fail_If(not Check_GF8.Check, "GF8");

   Status.New_Test;
   Status.Fail_If(not Check_GF16.Check, "GF16");

   Status.New_Test;
   Status.Fail_If(not Check_GF32.Check, "GF32");

   Status.Final_Report;
end Check_Gf;

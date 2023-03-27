with Text_Io; use Text_Io;
with GF_2p;
with GF_Generator;
with Check_Field;
with Ada.Command_Line;
use  Ada.Command_Line;
procedure Vero_Prova_Gf is
   -- 8, 11, 14
   Esp : constant := 2;--###

   package GF_Small is new GF_2p(Esp, Small_Footprint => True);
   subtype GFS is GF_Small.Galois;
   use GF_Small;

   package GF_Large is new GF_2p(Esp, Small_Footprint => False);
   subtype GFL is GF_Large.Galois;
   use GF_Large;

   package GF_Small_Generator is new GF_Generator(Gf_Small, 8);
   subtype GFS_Generator is GF_Small_Generator.Generator;
   use GF_Small_Generator;

   package GF_Large_Generator is new GF_Generator(Gf_Large, 8);
   subtype GFL_Generator is GF_Large_Generator.Generator;
   use GF_Large_Generator;


   package Check_GF_Small is
      new Check_Field(Field     => GF_Small.Galois,
                      Zero      => GF_Small.Zero,
                      One       => GF_Small.One,
                      Name      =>
                        "GF(2^" & Integer'Image(Esp) & ") small",
                      Generator => GF_Small_Generator.Generator);

   package Check_GF_Large is
      new Check_Field(Field     => GF_Large.Galois,
                      Zero      => GF_Large.Zero,
                      One       => GF_Large.One,
                      Name      =>
                        "GF(2^" & Integer'Image(Esp) & ") large",
                      Generator => GF_Large_Generator.Generator);
   -- tempi:
   --   esp=10, small_footpring=true  => 32 minuti
   --   esp=10, small_footpring=false => 12 minuti
begin
   if (Check_Gf_Large.Check and Check_Gf_Small.Check) then
      Put_Line ("OK");
      Set_Exit_Status(Success);
   else
      Put_Line ("FAIL");
      Set_Exit_Status(Failure);
   end if;
end Vero_Prova_Gf;

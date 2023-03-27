with Ada.Numerics.Discrete_Random;

package body Gf_Generator is
   subtype Base_Int is Unsigned_32 range 0..Unsigned_32(GF.Size-1);

   package Random_Int is
      new Ada.Numerics.Discrete_Random(Base_Int);

   Rnd_Gen : Random_Int.Generator;

   procedure Set_Class (G     : in out Generator;
                        Class :        Generator_Class)
   is
   begin
      G.Class := Class;
   end Set_Class;

   procedure Reset (G : in out Generator)
   is
      subtype No_Default_Class is
        Generator_Class range Full..Random;

      Actual_Class : No_Default_Class;
      N_Steps      : Natural;
   begin
      if (G.Class /= Default) then
         Actual_Class := G.Class;
      else
         if (GF.Exponent <= Max_Exponent_For_Full) then
            Actual_Class := Full;
         else
            Actual_Class := Random;
         end if;
      end if;

      case Actual_Class is
         when Full =>
            N_Steps := Integer(GF.Size);
         when Random =>
            N_Steps := 300;
      end case;

      G := (Class     => Actual_Class,
            Current   => GF.Zero,
            Contatore => 1,
            Massimo   => N_Steps);
   end Reset;

   procedure Next  (G : in out Generator)
   is
   begin
      if (G.Contatore < G.Massimo) then
         if (G.Class = Full) then
            G.Current := GF.To_Galois(G.Contatore);
         else
            G.Current := Gf.To_Galois(Unsigned_64(Random_Int.Random(Rnd_Gen)));
         end if;

         G.Contatore := G.Contatore + 1;
      end if;
   end Next;

   function  Again (G : in     Generator) return Boolean
   is
   begin
      return (G.Contatore < G.Massimo);
   end Again;

   function  Value (G : in     Generator) return GF.Galois
   is
   begin
      return G.Current;
   end Value;

end Gf_Generator;

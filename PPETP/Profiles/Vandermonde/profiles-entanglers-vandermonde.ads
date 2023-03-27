with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with profiles.builders.vandermonde;     use profiles.builders.vandermonde;

with Packets;   use Packets;


package  profiles.entanglers.vandermonde is
   -- ============= --
   -- = Entangler = --
   -- ============= --

   type Entangler is new Root_Entangler with private;
   type Entangler_Pt is access Entangler;

   No_Valid_Parameters: exception;

   function New_Entangler return Entangler_Pt;

   procedure Set_Default (Handler    : in out Entangler;
                          Parameters : in     Parameters_Class_Pt);


   procedure Entangle (Handler : in out Entangler;
                       Input   : in     Application_Packet;
                       Raw_Result  :    out Raw_Data); --Entangled_Data);

private
   type Entangler    is new Root_Entangler    with record
      Internal_Builder : Builder_Pt := New_Builder;
   end record;

end profiles.entanglers.vandermonde;

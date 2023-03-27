package  profiles.entanglers.basic is
   -- ============= --
   -- = Entangler = --
   -- ============= --

   type Entangler is new Root_Entangler with private;
   type Entangler_Pt is access Entangler;

   function New_Entangler return Entangler_Pt;

   procedure Set_Default (Handler    : in out Entangler;
                          Parameters : in     Parameters_Class_Pt);


--     procedure Entangle (Handler : in out Entangler;
--                         Input   : in     Application_Packet;
--                         Result  :    out Entangled_Data);

    procedure Entangle
     (Handler : in out Entangler;
      Input   : in Application_Packet;
      Result  : out Raw_Data);

private
   type Entangler    is new Root_Entangler    with null record;

end profiles.entanglers.basic;

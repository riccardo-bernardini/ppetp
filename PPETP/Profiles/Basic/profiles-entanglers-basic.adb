with Byte_Arrays;               use Byte_Arrays;
with Profiles.Entangled.Basic;  use Profiles.Entangled.Basic;

package body profiles.entanglers.basic is
   -- =================== --
   -- ==== ENTANGLER ==== --
   -- =================== --

   -------------------
   -- New_Entangler --
   -------------------

   function New_Entangler return Entangler_Pt is
   begin
      return new Entangler;
   end New_Entangler;

   -----------------
   -- Set_Details --
   -----------------

   procedure Set_Default
     (Handler    : in out Entangler;
      Parameters : in     Parameters_Class_Pt)
   is
   begin
      null;
   end Set_Default;



   --------------
   -- Entangle --
   --------------

--     procedure Entangle
--       (Handler : in out Entangler;
--        Input   : in     Application_Packet;
--        Result  :    out Entangled_Data)
--     is
--        Buf : Byte_Array_Pt := new Byte_Array'(Input.Buffer);
--
--        Payload : Entangled_Payload_Pt :=
--                    Entangled_Payload_Pt'
--                      (new Basic_Ent'(Bin_Data   => Buf,
--                                      My_Profile => Basic_Profile));
--     begin
--        Result := (Timestamp => Input.Timestamp,
--                   Payload   => Payload);
--     end Entangle;


   procedure Entangle
     (Handler : in out Entangler;
      Input   : in Application_Packet;
      Result  : out Raw_Data) is

      Buf : Byte_Array_Pt := new Byte_Array'(Input.Buffer);
   begin

      --Result.Inline := False;
      --Result.Flags  := no_flags;
      Result.Data := Buf;

   end Entangle;




end profiles.entanglers.basic;

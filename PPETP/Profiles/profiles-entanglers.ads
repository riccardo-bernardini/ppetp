with Profiles.Entangled;     use Profiles.Entangled;
with Profiles.Parameters;    use Profiles.Parameters;
with Packets.Binary.Application;    use Packets.Binary.Application;
with Packets.Protocol.Data;         use Packets.Protocol.Data;
package Profiles.Entanglers is

   -- =============== --
   -- == Entangler == --
   -- =============== --

   type Root_Entangler is abstract new Root_Profile_Handler with private;
   type Entangler_Class_Pt is access all Root_Entangler'Class;

   function New_Entangler
     (Profile    : Profile_Type;
      Parameters : Parameters_Class_Pt := null)
      return       Entangler_Class_Pt;


   procedure Entangle
     (Handler : in out Root_Entangler;
      Input   : in Application_Packet;
      Result  : out Raw_Data)--Entangled_Data)
   is abstract;

   procedure Set_Default (Handler    : in out Root_Entangler;
                          Parameters : in     Parameters_Class_Pt);

   function Get_Default (Handler : Root_Entangler)
                        return Parameters_Class_Pt;
private
   type Root_Entangler is abstract
     new Root_Profile_Handler with
      record
         Default : Parameters_Class_Pt;
      end record;
end Profiles.Entanglers;

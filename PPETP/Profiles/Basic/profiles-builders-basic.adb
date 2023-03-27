with Profiles.Entangled.Basic;   use Profiles.Entangled.Basic;
package body profiles.builders.basic is
   -- ================= --
   -- ==== BUILDER ==== --
   -- ================= --

   -----------------
   -- New_Builder --
   -----------------

   function New_Builder
     return Builder_Pt
   is
   begin
      return new Builder;
   end New_Builder;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (Handler : in out Builder;
                          Default : in     Parameters_Class_Pt)
   is
   begin
      null;
   end Set_Default;


   -----------------
   -- Format_Data --
   -----------------

   procedure Format_Data
     (Handler : in     Builder;
      Source  : in     Entangled_Payload_Pt;
      Inline  :    out Boolean;
      Flags   :    out Profile_Flags;
      Result  :    out Byte_Array_Pt)
   is
   begin
      Inline := False;
      Flags  := 0;
      Result := Basic_Ent(Source.all).Bin_Data;
   end Format_Data;

   -----------------------
   -- Format_Parameters --
   -----------------------

   procedure Format_Parameters
     (Handler    : in     Builder;
      Parameters : in     Parameters_Class_Pt;
      Result     :    out Byte_Array_Pt)
   is
   begin
      Result := new Byte_Array(1..0);
   end Format_Parameters;

end profiles.builders.basic;

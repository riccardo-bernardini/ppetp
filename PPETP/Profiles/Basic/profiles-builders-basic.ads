package  profiles.builders.basic is
   -- ============= --
   -- == Builder == --
   -- ============= --

   type Builder is new Root_Builder with private;
   type Builder_Pt is access Builder;

   function New_Builder return Builder_Pt;

   procedure Set_Default (Handler : in out Builder;
                          Default : in     Parameters_Class_Pt);

   procedure Format_Data (Handler : in     Builder;
                          Source  : in     Entangled_Payload_Pt;
                          Inline  :    out Boolean;
                          Flags   :    out Profile_Flags;
                          Result  :    out Byte_Array_Pt);
   -- This procedure is in some sense the opposite of Parse_Data
   -- in the sense that it takes profile data and parameters and creates
   -- the payload + profile header section of the Data packet.
   -- Profile flags and Inline flag are returned too.


   procedure Format_Parameters (Handler    : in     Builder;
                                Parameters : in     Parameters_Class_Pt;
                                Result     :    out Byte_Array_Pt);
   -- This procedure is the opposite of Parse_Parameters in the
   -- sense that it takes a set of profile parameters and it returns
   -- the payload part of a Set_Default packet.
private
   type Builder      is new Root_Builder      with null record;
end profiles.builders.basic;

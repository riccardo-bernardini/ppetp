with Ada.Unchecked_Deallocation;

with Profiles.Parameters;    use Profiles.Parameters;
with Profiles.Entangled;     use Profiles.Entangled;

with Byte_Arrays;            use Byte_Arrays;

--
-- This package defines the abstract version of a profile builder.
-- A profile builder is an object used to map entangled packets and
-- profile parameters into parts of Data and Control packets.
--

package Profiles.Builders is

   -- ============= --
   -- == Builder == --
   -- ============= --

   -- Root abstract type and corresponding class access
   type Root_Builder is abstract new Root_Profile_Handler with null record;
   type Builder_Class_Pt is access all Root_Builder'Class;

   -- Get a builder for a given profile
   function New_Builder (Profile : Profile_Type) return Builder_Class_Pt;


   -- A Builder_Table is a convenient way to keep at hand one
   -- builder for every profile
   type Builder_Table is array (Profile_Type) of Builder_Class_Pt;
   function New_Builder_Table return Builder_Table;


   procedure Finalize (Object : in out Builder_Table);

   procedure Set_Default
     (Handler : in out Root_Builder;
      Default : in Parameters_Class_Pt)
   is abstract;
   -- Set the default parameters for the builder Handler

   procedure Format_Data
     (Handler : in     Root_Builder;
      Source  : in     Entangled_Payload_Pt;
      Inline  :    out Boolean;
      Flags   :    out Profile_Flags;
      Result  :    out Byte_Array_Pt)
   is abstract;
   -- This procedure is in some sense the opposite of Parse_Data
   -- in the sense that it takes profile data and parameters and creates
   -- the payload + profile header section of the Data packet.
   -- Profile flags and Inline flag are returned too.

   procedure Format_Parameters
     (Handler    : in     Root_Builder;
      Parameters : in     Parameters_Class_Pt;
      Result     :    out Byte_Array_Pt)
   is abstract;
   -- This procedure is the opposite of Get_Full_Parameters in the
   -- sense that it takes a set of profile parameters and it returns
   -- the payload part of a Set_Default packet.

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Root_Builder'Class,
                                     Name   => Builder_Class_Pt);
end  Profiles.Builders;

with Interfaces, Galois_Matrices, Ada.Streams, Packets.Raw;
use  Interfaces, Galois_Matrices, Ada.Streams, Packets.Raw;

with Parsing_Utilities, PPETP;
use  Parsing_Utilities, PPETP;

with Profiles.Generic_Parser;

with Unchecked_Conversion, System;

package body Profiles.Vandermonde is
   Padding_Mask : constant Profiles.Flags := 2#001#;

   type Unsigned_6 is mod 2**6;

   ------------------------------------------------------------------
   -- Definition and representation of the (full) reduction header --
   ------------------------------------------------------------------

   type Full_Reduction_Header is
     record
        Reserved_1       : Unsigned_16;           -- Bytes 0 & 1

        Reserved_2       : Unsigned_6;            -- Byte  2
        Galois_Field     : GF_Name;

        Reduction_Factor : Reduction_Factor_Type; -- Byte 3
        Reduction_Vector : RV_Index;              -- Bytes 4..7
     end record;

   for Full_Reduction_Header'Bit_Order use System.High_Order_First;
   for Full_Reduction_Header use
     record
        Reserved_1       at 0 range 0..15;

        Reserved_2       at 2 range 0..5;
        Galois_Field     at 2 range 6..7;

        Reduction_Factor at 3 range 0..7;

        Reduction_Vector at 4 range 0..31;
     end record;

   Full_Header_Size   : constant Stream_Element_Offset := 8;

   subtype Full_Header_Buffer is Raw_Packet (1..Full_Header_Size);

   procedure Extract_Full_Header is
      new Extract (Target => Full_Reduction_Header);

   function To_Full_Header is
      new Unchecked_Conversion (Source => Full_Header_Buffer,
                                Target => Full_Reduction_Header);

   function To_Full_Buffer is
      new Unchecked_Conversion (Source => Full_Reduction_Header,
                                    Target => Full_Header_Buffer);


   function Foo2 return Integer is
   begin
      return 0;
   end Foo2;

   ---------------------------------------------------------------------
   -- Definition and representation of the (compact) reduction header --
   ---------------------------------------------------------------------

   type Compact_Reduction_Header is
     record
        Reduction_Vector : RV_Index;
     end record;

   for Compact_Reduction_Header'Bit_Order use System.High_Order_First;
   for Compact_Reduction_Header use
     record
        Reduction_Vector at 0 range 0..31;
     end record;

   Compact_Header_Size   : constant stream_element_offset := 4;


   function To_Packet (Data  : Matrix;
                       Unpad : Boolean)
                      return Raw_Packet is
      Result : Raw_Packet(1..2);
   begin
      return Result;
   end To_Packet;

   function To_Payload (Data    : Raw_Packet;
                        Details : Details_Type;
                        Pad     : Boolean      := False)
                       return Payload is
   begin
      return Zero(1);
   end To_Matrix;


   function Inline_Details (Current : Details_Type)
                           return Raw_Packet is
      subtype Compact_Header_Buffer is
        Raw_Packet (1..Compact_Header_Size);

        function To_Byte is
           new Unchecked_Conversion (Source => Compact_Reduction_Header,
                                     Target => Compact_Header_Buffer);
   begin
      return To_Byte ((Reduction_Vector => Current.Reduction_Vector));
   end Inline_Details;



   function Details_Needed (Current : Details_Type;
                            Default : Details_Type)
                           return Boolean is
   begin
      -- The only detail that can change is the reduction
      -- vector.  Reduction factor and Galois field must remain
      -- constant.
      if (Current.Galois_Field /= Default.Galois_Field or
            Current.Reduction_Factor /= Default.Reduction_Factor) then
         raise Uncompatible_Details;
      end if;

      return (Current.Reduction_Vector /= Default.Reduction_Vector);
   end Details_Needed;

   procedure Get_Details (From    : in out Parsing_Buffer;
                          Inline  : in     Boolean;
                          Flags   : in     Profiles.Flags;
                          Default : in     Details_Type;
                          Result  : in out Details_Type) is
      procedure Extract_Compact_Header is
         new Extract (Target => Compact_Reduction_Header);

      Padding : Boolean := (Flags and Padding_Mask) /= 0;
   begin
      if (Inline) then
         Result := Default;
         Result.Padded := Padding;
      else
         declare
            Compact : Compact_Reduction_Header;
         begin
            Extract_Compact_Header (From, Compact);

            Result := (Galois_Field     => Default.Galois_Field,
                       Reduction_Factor => Default.Reduction_Factor,
                       Reduction_Vector => Compact.Reduction_Vector,
                       Padded           => Padding);
         end;
      end if;
   end Get_Details;

   procedure Get_Full_Details (Source  : in out Parsing_Buffer;
                               Details :    out Details_Type) is
      Full : Full_Reduction_Header;
   begin
      Extract_Full_Header(Source, Full);

      Details := (Galois_Field     => Full.Galois_Field,
                  Reduction_Factor => Full.Reduction_Factor,
                  Reduction_Vector => Full.Reduction_Vector,
                  Padded           => False);

   end Get_Full_Details;

   function Make_Flags(Current : Details_Type)
                      return Profiles.Flags is
   begin
      if (Current.Padded) then
         return Padding_Mask;
      else
         return 0;
      end if;
   end Make_Flags;

   procedure Parse_Field  (Name   : in     String;
                           Value  : in     String;
                           Result :    out Parameters_Type;
                           Ok     :    out Boolean;
                           Reason :    out Config_Err_Reason) is
   begin
      raise Program_Error;
   end Parse_Field;

   procedure Parse_Factor (Name   : in     String;
                           Value  : in     String;
                           Result :    out Parameters_Type;
                           Ok     :    out Boolean;
                           Reason :    out Config_Err_Reason) is
   begin
      raise Program_Error;
   end Parse_Factor;

   procedure Parse_Vector (Name   : in     String;
                           Value  : in     String;
                           Result :    out Parameters_Type;
                           Ok     :    out Boolean;
                           Reason :    out Config_Err_Reason) is
   begin
      raise Program_Error;
   end Parse_Vector;

   procedure  Parse (Parser : in out Config_Parser;
                     Input  : in     Config_Table;
                     Result :    out Parameters_Class_Pt;
                     Errors : in out Config_Error_List) is
      package My_Parser is
         new Generic_Parser (Param_Name      => Param_Name,
                             Parameters_Type => Parameters);

      function UString(X : String) renames To_Unbounded_String;

      Info_Table : Parser_Info_Table :=
        (Field_Name     => (Name      => Ustring("galois-field"),
                            Callback  => Parse_Field'Access,
                            Mandatory => True),
         Reduction_Fact => (Name      => Ustring("reduction-factor"),
                            Callback  => Parse_Factor'Access,
                            Mandatory => True),
         Reduction_Vect => (Name      => Ustring("reduction-vector"),
                            Callback  => Parse_Vector'Access,
                            Mandatory => False));

   begin
      My_Parser.Parse (Info_Table => Info_Table,
                       Table      => Input,
                       Result     => Result,
                       Errors     => Errors);
   end Parse;
end Profiles.Vandermonde;

--                              -*- Mode: Ada -*-
--  Filename        : ppetp-attributes.ads
--  Description     : Root package for PPETP Attributes
--  Author          : Roberto Cesco Fabbro
--  Created On      : Mon May 03 14:19:13 2010
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Untested

with byte_arrays;	use byte_arrays;
with Parsing_Buffers;	use Parsing_Buffers;

with Ada.Unchecked_Deallocation;

package PPETP.Attributes is

   type Attributes_Index_Type is new Natural range 0 .. 255;

   type Root_Attributes is abstract tagged private;

   type Access_Attribute_Class is access
     all Root_Attributes'Class;





   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is abstract;
   function Building_Data(Object: Root_Attributes) return byte_array is abstract;

   type Handler_Processing is
     access function(Data: byte_Array) return Access_Attribute_Class;

   Yet_Registered_Index : exception;

   procedure Register(Index    : Attributes_Index_Type;
                      Callback : Handler_Processing);

   Unknown_Attributes : exception;

   procedure Get_Next_Attribute(Buffer : in out Parsing_Buffer;
                                Result :    out Access_Attribute_Class);


   function Get_Attribute_Packet(Object: Access_Attribute_Class) return byte_array;

   function Get_Type(Object: Access_Attribute_Class) return Attributes_Index_Type;

   procedure Free(X: in out Access_Attribute_Class);


private
   type Root_Attributes is abstract tagged
      record
         Attr_Type : Attributes_Index_Type;
      end record;

end PPETP.Attributes;

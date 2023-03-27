--                              -*- Mode: Ada -*-
--  Filename        : other_input_applications.ads
--  Description     : Root package for PPETP Other Input Applications
--  Author          : Roberto Cesco Fabbro
--  Created On      : Thu Jun 17 16:40:13 2010
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Untested

with byte_arrays;	use byte_arrays;

package Other_Input_Applications is



   type Application_Index_Type is new Natural range 0 .. 255;


   Yet_Registered_Index : exception;

   function Manage_Unknown_Packet(Data: Byte_Array_Pt) return Boolean;

   type Handler_Processing is
     access function(Data: Byte_Array_Pt) return Boolean;

   procedure Register(Index    : Application_Index_Type;
                      Callback : Handler_Processing);


end Other_Input_Applications;

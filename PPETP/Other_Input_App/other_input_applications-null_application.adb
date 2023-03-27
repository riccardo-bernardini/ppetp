with Other_Input_Applications;	use Other_Input_Applications;

-- Only for debug
with Ada.Text_IO;	use Ada.Text_IO;

package body Other_Input_Applications.NULL_Application is

   function Get_Data(Data: Byte_Array_Pt) return Boolean is
   begin

      Put_Line(" ***???*** NULL APPLICATION  ***???***");
      return True;
   end Get_Data;


begin
   Other_Input_Applications.Register(Index    => NULL_Application_Index,
                                     Callback => Get_Data'Access);
end Other_Input_Applications.NULL_Application;

with Byte_Arrays;            use Byte_Arrays;
with Auth.Profiles;          use Auth.Profiles;
with Ada.Finalization;       use Ada.Finalization;
with Network;                use Network;

package Auth.Credentials is
   -- ===================== --
   -- == Credential_Type == --
   -- ===================== --
   type Auth_Data is new Controlled with private;

   No_Auth : constant Auth_Data;

   function New_Credential (Data    : Byte_Array;
                            Profile : Auth_Profile)
                            return Auth_Data;

   function Data (X : Auth_Data)
                  return Byte_Array;

   function Profile (X : Auth_Data)
                     return Auth_Profile;

   function Image (X : Auth_Data) return String;

   function "=" (X, Y : Auth_Data) return Boolean;
private
   type Auth_Data is
     new Controlled with
      record
         Payload    : Byte_Array_Pt := null;
         My_Profile : Auth_Profile := Void_Profile;
      end record;


   overriding
   procedure Finalize (X : in out Auth_Data);

   overriding
   procedure Initialize (X : in out Auth_Data);

   overriding
   procedure Adjust (X : in out Auth_Data);

   No_Auth : constant Auth_Data := (Controlled with
                                    Payload    => null,
                                    My_Profile => Void_Profile);
end Auth.Credentials;

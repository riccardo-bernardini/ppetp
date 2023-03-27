package body Auth.Credentials is

   --------------------
   -- New_Credential --
   --------------------

   function New_Credential
     (Data    : Byte_Array;
      Profile : Auth_Profile)
      return Auth_Data
   is
   begin
      return (Controlled with
              Payload    => new Byte_Array'(Data),
              My_Profile => Profile);
   end New_Credential;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Auth_Data)
   is
   begin
      Free (X.Payload);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (X : in out Auth_Data) is
     New_Buffer : Byte_Array_Pt;
   begin
      if (X.Payload /= null) then
         New_Buffer := new Byte_Array'(X.Payload.all);
         X.Payload := New_Buffer;
      end if;
   end Adjust;

   function Data (X : Auth_Data)
                  return Byte_Array is
   begin
      return X.Payload.all;
   end Data;

   function Profile (X : Auth_Data)
                     return Auth_Profile is
   begin
      return X.My_Profile;
   end Profile;

   ---------
   -- "=" --
   ---------

   function "=" (X, Y : Auth_Data) return Boolean is
   begin
      if (X.Payload = null) then
         if (Y.Payload = null) then
            return True;
         else
            return False;
         end if;
      else
         if (Y.Payload = null) then
            return False;
         else
            return X.My_Profile = Y.My_Profile and
              X.Payload.all = Y.Payload.all;
         end if;
      end if;
   end "=";

   -----------
   -- Image --
   -----------

   function Image (X : Auth_Data) return String is
   begin
      if (X.Payload = null) then
         return "Auth-profile: "
           & Auth_Profile'Image (X.My_Profile)
           & "[]";
      else
         return "Auth-profile: "
           & Auth_Profile'Image(X.My_Profile)
           & Dump (X.Payload.all, True);
      end if;
   end Image;

   procedure Initialize (X : in out Auth_Data) is
   begin
      X.Payload := null;
      X.My_Profile := Void_Profile;
   end Initialize;
end Auth.Credentials;

with Auth.Credentials;          use Auth.Credentials;
with Network;                   use Network;

package Auth.Checkers is

   -- ================== --
   -- == Root_Checker == --
   -- ================== --
   type Credential_Type is
      record
         Requester  : Sock_Addr_Type;
         Target     : Sock_Addr_Type;
         Credential : Auth_Data;
      end record;

   type Root_Checker is abstract tagged null record;
   type Checker_Class_Pt is access Root_Checker'Class;

   procedure Check
     (Checker    : in out Root_Checker;
      Credential : in     Credential_Type;
      Result     :    out Boolean)
   is abstract;
end Auth.Checkers;

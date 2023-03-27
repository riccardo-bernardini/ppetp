--                              -*- Mode: Ada -*-
--  Filename        : auth_profiles-void.ads
--  Description     : Checker for the void profile
--  Author          :
--  Created On      : Tue Oct  7 16:52:57 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Tested: ok.

--
-- This package provides definitions for the Void
-- authentication profile, that is, the profile which
-- accepts everything.
--

package Auth.Profiles.Void is
   type Void_Checker is new Root_Checker with null record;
   type Void_Checker_Pt is access Void_Checker;

   procedure Check
     (Checker    : in out Void_Checker;
      Credential : in     Credential_Type;
      Result     :    out Boolean);


   function New_Checker
     return Void_Checker_Pt;
end Auth.Profiles.Void;

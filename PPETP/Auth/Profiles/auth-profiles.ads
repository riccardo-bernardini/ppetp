--                              -*- Mode: Ada -*-
--  Filename        : auth_profiles.ads
--  Description     : Root package for authentication profiles
--  Author          : Riccardo Bernardini
--  Created On      : Wed Oct  8 09:56:22 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
--
-- ===================
-- == What is this? ==
-- ===================
--
-- This package is the ancestor of the family of the packages
-- which implement authentication profiles.  An authentication
-- profile is implemented via a "Checker" type which must be
-- a descendant of abstract type Root_Checker defined in
-- this package.  Descendants of Root_Checker must override
-- method Check which accepts the credentials as a
-- Stream_Element_Array  and return True or False, depending
-- on the credentials validity.
--
-- ====================================
-- == How do I define a new profile? ==
-- ====================================
--
--   1) Choose a new profile Name + Number
--   2) In a child of this package
--      2.1) Derive the new Checker from Root_Checker
--      2.2) Override abstract method Check
--      2.3) Define a creator for the new Checker
--

package Auth.Profiles is
   -- =================== --
   -- == Profile types == --
   -- =================== --

   type Auth_Profile is (Void_Profile, Token_Profile, Signed_Profile);
   for Auth_Profile use (Void_profile   => 0,
                         Token_Profile  => 1,
                         Signed_Profile => 2);
   for Auth_Profile'Size use 8;

end Auth.Profiles;

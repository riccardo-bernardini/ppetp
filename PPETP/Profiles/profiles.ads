
--
-- As described in the specs, PPETP is made of two layers:
-- a "transport layer" (which explains the packet format)
-- and a "profile" (which describes how data is processed
-- to construct the packets to be transmitted).
--
-- At this moment (September 18, 2008) only two profiles are
-- defined: Basic (which does no processing at all) and
-- Vandermonde (which "reduces" the data by multiplication
-- with a Vandermonde matrix).
--
-- As described in the PPETP document, a profile specification
-- must include
--
--   1. The profile number
--   2. The processing applied to the data
--   3. How profile-specific data are included in the packet, i.e.
--       3.a The meaning of the Marker field
--       3.b The payload of Set_Default
--       3.c The format of profile-specific header when the
--           Inline flag is set
--
-- From a practical point of view, we implement such a flexibility
-- by defining several abstract tagged "handlers", one for each duty
-- requested by the profile.  More precisely, the abstract handlers
-- associated to a profile are
--
--   A. "Passive" types (i.e., record which just store informations
--      and have no method of their own)
--
--      A1. A "parameter" type used to store the parameters associated
--          to a profile
--
--      A2. An "entangled" type which represent a processed packet.
--
--   B. "Active" types (i.e., objects which are required to do some
--       type of action)
--
--      B1. A "builder" to transform profile parameters into
--          the payload of the Set_Default command
--
--      B2. A "parser" which interprets the payload of the binary
--          packet received from the network and return a profile
--          parameter description or an entangled record
--
--      B3. An "entangler" to process the packet
--
--      B4. A "disentangler" to reconstruct the packets.
--
--      B5. A "configuration parser"
--
--      B6. An "handshaker"
--
-- Those abstract types are defined in child packages, one child
-- per type (with names like "Profiles.Builders").  Their methods
-- will be implemented as abstract primitive operations (see RM).
--
-- In order to introduce a new profile (e.g. New_Prof), the programmer
-- must
--
--  1. For every abstract handler
--    1.1. Derive from the abstract handler a "concrete" handler.
--         It is suggested that the derive handler is defined
--         in a child of the package which defines the abstract
--         handler (e.g., Profiles.Builders.New_Prof)
--
--    1.2. Write new data handling procedures which override
--         the ones of the abstract handler
--
--  2. Update the Profile_Type type in the root package
--
--  -- MbO = maybe obsolete
--  -- MbO 3. Update the New_*_Table functions in the child
--  -- MbO   packages of Profile (if some function is not
--  -- MbO   updated, the compiler will signal the error)
--

package Profiles is
   type Profile_Flags is mod 2 ** 3;
   for Profile_Flags'Size use 3;

   -- Reduction profiles (currently only  profiles Basic
   -- and Vandermonde are defined)
   type Profile_Type is (Basic_Profile, Vandermonde_Profile);
   for Profile_Type use (Basic_Profile => 0 , Vandermonde_Profile => 1);

   for Profile_Type'Size use 4;

   type Root_Profile_Handler is abstract tagged private;



private

   type Root_Profile_Handler is tagged  null record;

end Profiles;

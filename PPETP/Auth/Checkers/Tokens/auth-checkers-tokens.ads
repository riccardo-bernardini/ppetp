--                              -*- Mode: Ada -*-
--  Filename        : auth_profiles-tokens.ads
--  Description     : Checker for token profile
--  Author          :
--  Created On      : Tue Oct  7 16:51:59 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <Tested>

--
-- ===================
-- == What is this? ==
-- ===================
--
-- This package provides definitions for the Tokens
-- authentication profile.  Within this profile, a trusted
-- authority gives us a list of "tokens" (64 bit words).
-- A request is accepted if it is accompanied by a token
-- belonging to the list.  Each token can be used at most
-- once.
--
-- ======================
-- == How do I use it? ==
-- ======================
--
-- Checking is done as with any other checker.  In order to
-- create a new checker the function New_Checker must be used.
-- Function New_Checker accepts as parameter the list of valid
-- tokens.
--

package Auth.Checkers.Tokens is
   -- Type representing a token
   Token_Bit_Size : constant Stream_Element_Offset := 64;
   type Token_Type is
     new Stream_Element_Array (1 .. Token_Bit_Size / Stream_Element'Size);

   type Token_List is array (Positive range <>) of Token_Type;

   -- Checker for the Token profile
   type Token_Checker(<>) is new Root_Checker with private;
   type Token_Checker_Pt is access Token_Checker;

   -- Checker constructor
   function New_Checker (List : Token_List)
                        return Token_Checker_Pt;

   overriding
   procedure Check
     (Checker    : in out Token_Checker;
      Credential : in     Credential_Type;
      Result     :    out Boolean);

private
   type Token_Info is
      record
         Token : Token_Type;
         Used  : Boolean;
      end record;

   type Tk_Array is array (Positive range <>) of Token_Info;

   type Token_Checker (N_Tokens : Positive) is
     new Root_Checker with
      record
         Valid_Tokens : Tk_Array (1 .. N_Tokens);
         N_Used       : Natural;
      end record;
end Auth.Checkers.Tokens;

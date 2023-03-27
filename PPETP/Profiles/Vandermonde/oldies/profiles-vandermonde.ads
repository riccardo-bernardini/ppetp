--                              -*- Mode: Ada -*-
--  Filename        : profiles-medusa.ads
--  Description     : Definitions related to the Vandermonde profile
--  Author          : Finta Tartaruga
--  Created On      : Thu Sep 11 21:57:19 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- PPETP allows the packets sent over the P2P network to be processed
-- in some way.  PPETP does not dictate how the packet are processed,
-- delegating such decision to "profiles."  Currently, only the Vandermonde
-- profile (which reduces the packet via products of Galois matrices)
-- is defined.
--
-- In order to allow for the easy introduction of new profiles, the
-- profile "private" processing is collected in a single library. The
-- profile processing can be parametrized by some "profile details".
--
-- A profile library must provide functions to convert from/to
-- external binary packets, both payloads and headers.
--
with PPETP;

with Galois_Matrices, Ada.Streams, Packets.Network, Parsing_Utilities;
use  Galois_Matrices, Ada.Streams, Packets.Network, Parsing_Utilities;

limited private with Profiles.Vandermonde.Processing;

package Profiles.Vandermonde is
   type Payload is private;

   type Vandermonde_Handler is
     new Profile_Handler with null record;

   type Details_Type is
     new Profile_Details with private;

   type Data_Type is
     new Profile_Packet with private;

   overriding
     procedure Parse_Data (Handler : in     Vandermonde_Handler;
                           Default : in     Details_Class_Pt;
                           From    : in out Parsing_Buffer;
                           Flags   :    out Flags;
                           Inline  :    out Boolean;
                           Result  :    out Data_Class_Pt);

   overriding
     procedure Get_Full_Details (Handler : in     Vandermonde_Handler;
                                 Source  : in out Parsing_Buffer;
                                 Details :    out Details_Class_Pt);

   overriding
     procedure Make_Packet (Handler : in     Vandermonde_Handler;
                            Default : in     Details_Class_Pt;
                            Source  : in     Data_Class_Pt;
                            Inline  :    out Boolean;
                            Flags   :    out Flags;
                            Result  :    out Stream_Element_Array_Pt);

   overriding
     procedure Make_Details (Handler : in     Vandermonde_Handler;
                             Details : in     Details_Class_Pt;
                             Result  :    out Stream_Element_Array_Pt);

private
   type Payload_Pt is access Processing.Vandermonde_Payload;

   type Details_Type is
     new Profile_Details with
      record
         Galois_Field     : PPETP.GF_Name;
         Reduction_Factor : PPETP.Reduction_Factor_Type;
         Reduction_Vector : PPETP.RV_Index;
         Padded           : Boolean;
      end record;

   type Data_Type is
      record
         Details : Details_Type;
         Payload : Payload_Pt;
      end record;
end Profiles.Vandermonde;

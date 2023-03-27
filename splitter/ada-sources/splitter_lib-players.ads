--
-- ** What is this? **
--
-- This package provides a type (Player_Descriptor) used to represent
-- an external player inside RTP-splitter.  A player is characterized
-- by the following attributes
--
--    + A player ID (used to control the player via the control server)
--    + The UDP port listened by the player
--
-- ** What kind of actions are possible? **
--
-- No special actions are defined.  The "descriptor" is just a "passive"
-- data structure.
--
package Splitter_Lib.Players is
   type Player_ID is new Integer range -1 .. 2 ** 16 - 1;

   type Player_Descriptor is
      record
         ID   : Player_ID;
         Port : Natural;
      end record;

   No_Player : constant Player_Descriptor := (ID   => Player_ID'First,
                                              Port => 0);
end Splitter_Lib.Players;

--
-- This package provides just a single procedure which implements the
-- "core" of the splitter program.
--
with Splitter_Lib.Command_Parser;

package Splitter_Lib.Splitter is
   procedure Start (Config : Command_Parser.Config_Data);
end Splitter_Lib.Splitter;

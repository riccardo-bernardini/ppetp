--                              -*- Mode: Ada -*-
--  Filename        : network.ads
--  Description     : Socket-level package
--  Author          : Riccardo Bernardini
--  Created On      : Tue Aug 26 17:52:26 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>
--  Type            : <instance(GNAT.Socket)>

--
-- This package is simply a renaming of GNAT.Sockets.  This is done
-- for portability purposes, in order not to have lots of "GNAT.Sockets"
-- spread in the code.
--

with GNAT.Sockets;
package Network renames GNAT.Sockets;

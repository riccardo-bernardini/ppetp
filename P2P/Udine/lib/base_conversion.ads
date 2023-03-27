--                              -*- Mode: Ada -*-
--  Filename        : base_conversion.ads
--  Description     : Generic package to convert integers to strings
--  Author          : Finta Tartaruga
--  Created On      : Wed Jul  2 22:00:47 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Basic test OK

--
-- This is a very simple generic package to convert integer variables
-- to string.  I was not able to find anything similar inside the
-- standard Ada. Possible? (Yes, I know about the Text_IO package,
-- but the Put procedure add numeric quotes "#", while I was simply
-- interested in pure "number to string" conversion)
--
generic
   type Num is range <>;
package Base_Conversion  is
   subtype Number_Base is Integer range 2 .. 16;

   procedure To_String (Item     : in     Num;
                        To       :    out String;
                        Base     : in     Number_Base := 16;
                        Zero_Pad : in     Boolean     := True);
end Base_Conversion;

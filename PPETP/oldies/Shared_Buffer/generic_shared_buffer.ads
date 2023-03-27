--                              -*- Mode: Ada -*-
--  Filename        : generic_shared_buffer.ads
--  Description     : Low-weight fixed-size shared buffer
--  Author          :
--  Created On      : Mon Jul 21 17:42:48 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides a low-weight shared  buffer between
-- _a_single_reader_ and _a_single_writer_.
--
-- The main type provided by this package is Shared_Buffer which is a
-- record with two fields
--
--      Buffer : an array of Element (the formal type of this
--               package)
--
--      Index  : a protected objects which, on request, hands out
--               the index of first free/ready entry of Buffer.
--
-- How to use it:
--
--     Declare:
--
--        package Int_Buffer is
--           new Generic_Shared_Buffer(Integer);
--
--        My_Buf : Int_Buffer.Shared_Buffer(Size => 32);
--
--     Writer:
--
--        My_Buf.Index.Begin_Writing(First_Free);
--        My_Buf.Buffer(First_Free) := 42;
--        My_Buf.Index.Done_Writing(First_Free);
--
--     Reader:
--
--        My_Buf.Index.Begin_Reading(First_Ready);
--        Put(Integer'Image(Buffer(First_Free)));
--        My_Buf.Index.Done_Reading(First_Ready);
--
-- The buffer grants a FIFO behaviour.
--
generic
   type Element is private;
package Generic_Shared_Buffer is
   protected type Index_Handler(Size : Natural) is
      entry     Begin_Writing (Where :    out Natural);
      entry     Begin_Reading (Where :    out Natural);

      procedure Done_Writing  (Where : in     Natural);
      procedure Done_Reading  (Where : in     Natural);

      procedure Cancel_Writing  (Where : in     Natural);
      procedure Cancel_Reading  (Where : in     Natural);

      function Any_Ready return Boolean;
      function Full      return Boolean;

      function How_Many_Ready   return Natural;
      function How_Many_Free    return Natural;
      function How_Many_Entries return Natural;
   private
      First_Free  : Natural := 0;
      N_Ready     : Natural := 0;

      Writing     : Boolean;
      Reading     : Boolean;
   end Index_Handler;

   type Buffer_Type is array (Natural range <>) of Element;

   type Shared_Buffer(Size : Natural) is
      record
         Index  : Index_Handler (Size);
         Buffer : Buffer_Type (0..Size);
      end record;
end Generic_Shared_Buffer;

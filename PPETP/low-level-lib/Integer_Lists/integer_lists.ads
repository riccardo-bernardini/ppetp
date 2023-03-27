--                              -*- Mode: Ada -*-
--  Filename        : integer_lists.ads
--  Description     : Efficiently store a list of natural numbers
--  Author          : Finta Tartaruga
--  Created On      : Fri Aug 24 22:32:11 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

with Ada.Finalization;
use  Ada.Finalization;
--
-- A Done_List is a list which stores the timestamps of already
-- processed packets. The goal of storing the timestamps of processed
-- packets is to allow for discarding the packets with the same timestamp
-- which will arrive (maybe) in the future.
--
-- A Done_List has just two methods: Insert and Contains. Note that
-- a given timestamp cannot be inserted twice (Program_Error raised)
--
generic
   type Item_Type is mod <>;
package Integer_Lists is
   type Integer_List is new Controlled with private;

   type Duplicate_Policy is (Die, Ignore);

   procedure Insert (List         : in out Integer_List;
                     Item         : in     Item_Type;
                     On_Duplicate : in     Duplicate_Policy := Die);

   function Contains (List : in Integer_List;
                      Item : in Item_Type) return Boolean;
private
   type Item_Buffer is array (Item_Type range <>) of Boolean;
   type Item_Buffer_Ref is access Item_Buffer;

   type Integer_List is new Controlled with record
      First  : Item_Type;
      Buffer : Item_Buffer_Ref;
   end record;

   procedure Initialize(Object: in out Integer_List);
end Integer_Lists;

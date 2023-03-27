--                              -*- Mode: Ada -*-
--  Filename        : generic_tables.ads
--  Description     : Generic shared array
--  Author          :
--  Created On      : Mon Sep  8 13:01:08 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

--
-- This package provides a protected type which implements a
-- "protected table", useful as a table shared among different
-- tasks. The "model" of the implemented table is as follows
--
--  * The table is similar to an array in the sense that it has a
--    bounded number of entries (chosen with the procedure Resize) and
--    each entry is identified by an Index (of type Cursor).
--
--  * Each entry can be free or busy.  The table itself keeps track of
--    which entries are busy.
--
--  * A new entry is allocated with the procedures Reserve (which only
--    reserves the entry) and Insert (which also write the new
--    element).  An already allocated entry can be written with the
--    function Replace. Entries are freed with the function Delete.
--
--  * If procedures Reserve and Insert are called on a full table,
--    exception Table_Full is raised.
--
--
--
with Ada.Finalization;

generic
   type Element is private;
   type Cursor  is range <>;
package Generic_Tables is
   subtype Size_Type is Natural;

   type Callback_Procedure is
     access procedure (X : Element);

   type Callback_Function  is
     access function (X : Element) return Element;

   type Basic_Table is new
     Ada.Finalization.Controlled with private;

   type Boolean_Callback_Function  is
     access function (X : Element) return Boolean;


   protected type Table is
      procedure Reserve (Index : out Cursor);
      -- Reserve an entry in the table and return the corresponding
      -- index in Index.  The entry is left unitialized.
      -- Raises Table_Full if no new entry is available.

      procedure Replace (Index    : in Cursor;
                         New_Item : in Element);
      -- Write New_Item in the entry identified by Index.  Raise
      -- Invalid_Cursor if entry Index is not "busy".


      procedure Insert (Item  : in     Element;
                        Index :    out Cursor);
      -- Equivalent to calling both Reserve and Replace.  More
      -- precisely, T.Insert(Item, Index) is equivalent to the
      -- following piece of code
      --
      --      T.Reserve(Index);
      --      T.Replace(Index, Item);
      --
      -- with the difference that the call to Insert is atomic.
      -- Raises Table_Full if no new entry is available.


      procedure Delete (Index : Cursor);
      -- Make the entry at Index available.  Raises Invalid_Cursor if
      -- entry Index is not "busy"

      procedure Delete_If (Callback: Boolean_Callback_Function);
      -- Delete an element if the callback function return true

      procedure Iterate (Callback :      access procedure (X : Element));
      -- Call procedure Callback for every busy entry of the
      -- Table. This procedure is necessary in order to be able to
      -- process all the entries of the table in an atomic way.

      procedure Iterate (Callback : Callback_Function);
      -- Call function Callback for every busy entry of the Table.
      -- Replace each entry with the function result. This procedure
      -- is necessary in order to be able to process all the entries
      -- of the table in an atomic way.

      procedure Start_Iteration;
      function  Iterate_Again return Boolean;
      procedure Next_Iteration (Item : out Element);

      function Is_Full  return Boolean;
      -- Return true if the table is full

      function Is_Empty return Boolean;
      -- Return true if the table is empty or not initialized

      function Exists (Index : Cursor) return Boolean;
      -- return True if the entry as Index is busy.  Do not raise any
      -- exception.

      function Get (Index : Cursor) return Element;
      -- Return the element at Index.  Raise Invalid_Cursor is Index
      -- is not busy.

      procedure Resize (Size : Size_Type);
      -- (re-)Initialize the table with an empty table of Size
      -- entries.

      function Count return Size_Type;
      -- Return the number of element in the table

   private
      Buf : Basic_Table;
   end Table;


   Table_Full          : exception;
   Invalid_Cursor      : exception;
   Uninitialized_Table : exception;

private
   type Buffer_Entry is
      record
         Data : Element;
         Busy : Boolean := False;
      end record;

   subtype Cursor_Iter_Type is Integer range Integer(Cursor'First-1).. Integer(Cursor'Last);

   type Buffer_Array is array(Cursor range <>) of Buffer_Entry;
   type Buffer_Pt    is access Buffer_Array;

   type Basic_Table is new
     Ada.Finalization.Controlled with
      record
         Iter_Cursor : Cursor_Iter_Type;
         Size   : Size_Type := 0;
         N_Busy : Size_Type := 0;
         Buffer : Buffer_Pt := null;
      end record;

   overriding procedure Finalize (Object : in out Basic_Table);
end Generic_Tables;

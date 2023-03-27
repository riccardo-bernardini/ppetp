--                              -*- Mode: Ada -*-
--  Filename        : smart_records.ads
--  Description     : Tagged type for automatic deallocation
--  Author          : Riccardo Bernardini
--  Created On      : Wed Oct 22 09:31:10 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Informal test

--
-- This package provides a tagged type useful for implementing
-- reference-counting types.  In order to use it, the user must
-- derive a new type from Smart_Record.  A Smart_Record includes
-- a pointer to a counter which is incremented every time the
-- record is assigned and it is decremented every time the
-- record is finalized.  When the counter reachs 0, procedure
-- Destroy (typically overriden by the user) is called.
--
-- Example of how a "smart pointer" can be implemented by
-- means of this package
--
-- ***************
-- ** Spec file **
-- ***************
--
--    with Smart_Records;   use  Smart_Records;
--
--    package Smart_Integers is
--       type Access_Int is access Integer;
--
--       type Smart_Int is
--         new Smart_Record with
--          record
--            C : Access_Int;
--          end record;
--
--       procedure Destroy(X : in out Smart_Int);
--
--       procedure Initialize (X : in out Smart_Int);
--    end Smart_Integers;
--
-- ***************
-- ** Body file **
-- ***************
--
--    with Ada.Unchecked_Deallocation;
--
--    package body Smart_Integers is
--
--       procedure Destroy (X : in out Smart_Int) is
--          procedure Free is
--             new Ada.Unchecked_Deallocation (Integer, Aint);
--       begin
--          Free(X.C);
--       end Destroy;
--
--       procedure Initialize (X : in out Smart_Int) is
--       begin
--          Initialize(Smart_Record(X));
--          X.C := new Integer;
--       end Initialize;
--    end Smart_Integers;
--

with Ada.Finalization;    use  Ada.Finalization;

package Smart_Records is
   type Smart_Record is new Controlled with private;

   procedure Destroy(X : in out Smart_Record) is null;
private
   type Access_Natural is access Natural;

   type Smart_Record is new Controlled with
      record
         Id : Natural;
         Counter : Access_Natural;
      end record;

   procedure Initialize (Object : in out Smart_Record);
   procedure Adjust     (Object : in out Smart_Record);
   procedure Finalize   (Object : in out Smart_Record);
end Smart_Records;

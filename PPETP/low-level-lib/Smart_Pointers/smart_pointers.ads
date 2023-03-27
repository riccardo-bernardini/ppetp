--                              -*- Mode: Ada -*-
--  Filename        : smart_pointers.ads
--  Description     : Generic package for "smart" pointers
--  Author          : Riccardo Bernardini
--  Created On      : Wed Sep 17 12:19:06 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

--
-- = What is this package? =
-- =========================
--
-- This generic package allows the programmer to use "smart" pointers,
-- that is, pointers which keep track of how many references they have
-- and destroy the pointed object when the number of references reach
-- 0.
--
-- = How can I use it? =
-- =====================
--
-- In order to use smart pointers, the programmer must
--
--   1. Instantiate a specialized version of this package by
--      specifying
--
--        a) The type of the pointed object
--        b) An access type for the pointed object
--        c) A procedure to free dinamically allocated objects
--
--   2. New smart pointers are created by means of the function New_Pt
--      which requires an access to the allocated object
--
--   3. The access value passed to New_Pt can be recovered by means
--      of function Pt
--
-- == Example ==
-- =============
--
-- -- Skeleton of an example of smart pointers for integers
-- -- See the test program (in dir test) for a more complete
-- -- example of usage.
--
-- -- Access to object --
-- ----------------------
-- type Int_Access is access Integer;
--
-- -- Memory reclaiming function --
-- --------------------------------
-- procedure Free_Int is
--      new Ada.Unchecked_Deallocation(Object => Integer,
--                                     Name   => Int_Access);
--
-- -- Instatiate the new package --
-- --------------------------------
-- package Int_Smart_Pt is
--   new Smart_Pointers (Object        => Integer,
--                       Object_Access => Int_Access,
--                       Free          => Free_Int);
--
-- -- Allocate and initialize a new Smart_Pt --
-- --------------------------------------------
-- X : Int_Smart_Pt.Smart_Pt := Int_Smart_Pt.New_Pt(new Integer'(42));
-- Y : Int_Smart_Pt.Smart_Pt := X;
--
-- -- Use it --
-- ------------
-- Put_Line(Integer'Image(X.Pt.all));
--

with Ada.Finalization;

generic
   type Object is private;
   type Object_Access is access Object;
   with procedure Free(X : in out Object_Access) is <>;
package Smart_Pointers is
   type Smart_Pt is
     new Ada.Finalization.Controlled with private;

   function New_Pt (X : Object_Access) return Smart_Pt;

   function Pt (X : Smart_Pt) return Object_Access;
private
   type Obj_Info is
      record
         Count : Natural := 0;
         Data  : Object_Access;
      end record;

   type Obj_Info_Pt is access Obj_Info;

   type Smart_Pt is
     new Ada.Finalization.Controlled with
      record
         Pt : Obj_Info_Pt;
         ID : Natural;
      end record;

   procedure Finalize (Object : in out Smart_Pt);
   procedure Adjust   (Object : in out Smart_Pt);
end Smart_Pointers;


--                              -*- Mode: Ada -*-
--  Filename        : Address_List.ads
--  Description     : List of peers address with timeout
--  Author          : Roberto Cesco Fabbro
--  Created On      : Mon Feb 16, 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Very basic test Ok

-- This package provide a doubled linked-list whose elements are pair
-- < Peer_Address, Expiration_Time >. The elements are ordered in base of the
-- expiration time, the first element is the next expirtate element. If the list
-- is empty and the Get_Next_Expiration_Time function is called an Empty_List
-- exception is raised.

with Network; use Network;
with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Deallocation;

package Input.Address_List is



   type Peer_Table is tagged private;

   -- Set the timeout duration
   procedure Set_Timeout(List : in out Peer_Table;
                         How  : in Duration);

   -- Insert an item at the end of the list, if an item with the same address
   -- already exists, it would be deleted.
   procedure Insert(List : in out Peer_Table;
                    Item : in Inet_Addr_Type);

   -- Get the next expiration time; If the list is empty return an Empty_List
   -- exception
   function Get_Next_Expiration_Time(List : in Peer_Table) return Time;

   Empty_List : exception;

   -- Return the address of the element with an expired time and cancel it
   -- from the list
   procedure Get_And_Delete_Expired(List : in out Peer_Table;
                                    Addr : out Inet_Addr_Type);

   -- Return the size of the list
   function Size(List : in Peer_Table) return Natural;

   -- Delete all the elements of the list
   procedure Clear(List : in out Peer_Table);



private

   type Table_Item is record
      Addr    : Inet_Addr_Type;
      Expires : Time;
   end record;

   type Table_Record;
   type Table_Record_Pt is access Table_Record;

   type Table_Record is record
      Item : Table_Item;
      Next : Table_Record_Pt;
      Prev : Table_Record_Pt;
   end record;

   type Peer_Table is tagged record
      Timeout : Duration := 10.0;
      First   : Table_Record_Pt := null;
      Last    : Table_Record_Pt := null;
      Size    : Natural := 0;
   end record;



   procedure Find(List  : in out Peer_Table;
                  Item  : in  Inet_Addr_Type;
                  Point : out Table_Record_Pt);

   procedure Add_At_The_End(List  : in out Peer_Table;
                            Item  : in  Inet_Addr_Type);

   procedure Delete(List  : in out Peer_Table;
                    Where : in out Table_Record_Pt);


   procedure Free is new Ada.Unchecked_Deallocation (Table_Record, Table_Record_Pt);

   pragma Controlled(Table_Record_Pt);


end Input.Address_List;


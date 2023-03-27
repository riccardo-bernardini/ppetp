--                              -*- Mode: Ada -*-
--  Filename        : server_db.ads
--  Description     : db of peers identifiers ad address
--  Author          : Roberto Cesco Fabbro
--  Created On      : Gen, 22 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Development


-- This package provide a dynamic (linked list) DB for storing
-- pairs (peer_ID, peer_Address)

with GNAT.Sockets; use GNAT.Sockets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Peer_Info; use Peer_Info;
with Tokenize; use Tokenize;

package Server_DB is



   -- This is a DB item, it contains the peer identifier and its address
   type DB_Item is record
      ID: Peer_Identifier;
      Address: Peer_Address;
   end record;


   -- Exceptions
   peer_not_present: exception;
   peer_yet_present: exception;
   out_of_bound_index: exception;



   -- DB Class
   type Peer_DB is tagged private;


   -- Add an item to DB:
   --	if the item already raise a peer_yet_present exception
   procedure Add(db: in out Peer_DB; item: in DB_Item);

   -- Return the Address of the peer identified by the parameter peer_id
   -- 	if the item is not found raise a peer_not_present exception
   function Get(db: in Peer_DB; peer_id: in Peer_Identifier) return Peer_Address;

   -- Return the Item at position index ( from 1 to db.num_item)
   --	if the index is greater then the size of the list raise an
   --   out_of_bound_index exception
   function Get(db: in Peer_DB; index: Positive) return DB_Item;

   -- Delete the item identified by peer_id from the DB
   --	if the item doesn't exist raise a peer_not_present exception
   procedure Del(db: in out Peer_DB; peer_id: in Peer_Identifier);

   -- Return the number of items
   function Count(db: in Peer_DB) return integer;

   -- Print all the elements of the list
   -- procedure PrintAll(db: in Peer_DB);

   -- Return a string of the item
   function Image(item: DB_Item) return Unbounded_String;

   -- Return a DB_Item from a string generated by Image
   function Value(str: Unbounded_String) return DB_Item;

   -- Return a string of all the element in the db
   function Peer_DB_To_String(db: Peer_DB) return Unbounded_String;




private

   type DB_Element;
   type DB_Element_Access is access DB_Element;

   type DB_Element is record
      data: DB_Item;
      next: DB_Element_Access;
   end record;

   type Peer_DB is tagged record
      current: DB_Element_Access := null;
      num_item: Integer := 0;
   end record;

   Procedure Free is new Ada.Unchecked_Deallocation (DB_Element, DB_Element_Access);

   pragma Controlled(DB_Element_Access);

end Server_DB;

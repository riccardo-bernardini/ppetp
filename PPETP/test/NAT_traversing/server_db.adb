


package body Server_DB is


   procedure Add(db: in out Peer_DB; item: in DB_Item) is
      tmp: DB_Element_Access;
      no_used: Peer_Address;
   begin

      -- first check if the peer is yet present, it's used the Get function.
      -- it raise a peer_not_present exception if the item is not found
      begin
         no_used := Get(db, item.ID);

      exception

         when peer_not_present =>

            if db.num_item = 0 then  -- this is the first element of the list

               db.current := new DB_Element'(item,null);

            else

               tmp := new DB_Element'(item,null);

               tmp.next := db.current;

               db.current := tmp;

            end if;

            db.num_item := db.num_item + 1;

            return;
      end;

      -- if the program arrive here mean that Get not raised an exception hence
      -- mean that a peer with this Id is yet in the list
      raise peer_yet_present;

   end Add;



   function Get(db: in Peer_DB; peer_id: in Peer_Identifier) return Peer_Address is
      tmp: DB_Element_Access;

   begin
      tmp := db.current;

      -- No items in the list
      if db.num_item = 0 then
         raise peer_not_present;
      end if;



      while (tmp.next /= null) loop
         if tmp.data.id = peer_id then
            return tmp.data.Address;
         end if;
	     tmp := tmp.next;
      end loop;

      -- this is necessary becose the loop don't check the last element because
      -- the next pointer is null
      if tmp.data.id = peer_id then
         return tmp.data.Address;
      end if;

      -- if the program arrive here the item is not found

      raise peer_not_present;

   end Get;

   function Get(db: in Peer_DB; index: Positive) return DB_Item is
      tmp: DB_Element_Access;
   begin

      -- index too big
      if index > db.num_item then
         raise out_of_bound_index;
      end if;

      tmp := db.current;

      -- current is the last element hence to achieve the element located at
      -- position index we have to skip (db.num_item - index) elements
      for i in Positive range  1..(db.num_item - index) loop
         tmp := tmp.next;
      end loop;

      return tmp.data;
   end Get;



   procedure Del(db: in out Peer_DB; peer_id: in Peer_Identifier) is
      tmp: DB_Element_Access;
      previous_element: DB_Element_Access;
   begin



      tmp := db.current;
      previous_element := null;

      -- No items in the list
      if db.num_item = 0 then
         raise peer_not_present;
      end if;




      while (tmp.next /= null) loop
         if tmp.data.id = peer_id then

            -- If the item to delete is the current one it's necessary to change the
            -- current pointer too, but is not necessary to set the next pointer
            if tmp.data.id = db.current.data.id then
               db.current := db.current.next;
            else
               --  re-set the next pointer
               previous_element.next := tmp.next;
            end if;


            Free(tmp);

            db.num_item := db.num_item - 1;

            return;
         end if;

         previous_element := tmp;
         tmp := tmp.next;


      end loop;

      -- this is necessary becose the loop don't check the last element because
      -- the next pointer is null
      if tmp.data.id = peer_id then


         previous_element.next := null; -- no more element

         Free(tmp);

         db.num_item := db.num_item - 1;
         return;

      end if;

      -- if the program arrive here the item is not found

      raise peer_not_present;

   end Del;


   function Count(db: in Peer_DB) return integer is
   begin
      return db.num_item;
   end Count;


   function Image(item: DB_Item) return Unbounded_String is
      tmp: Unbounded_String;
   begin
      tmp := Image(item.ID);
      tmp := tmp & ",";
      tmp := tmp & To_Unbounded_String(Image(item.Address));
      return   tmp;
   end Image;

   function Value(str: Unbounded_String) return DB_Item is
      tmp: DB_Item;
      list: Token_List;
      addr_str: Unbounded_String;
   begin

      -- divide the Id and the address
      list := Split(To_String(str),',');

      tmp.ID := Peer_Identifier'Value(To_String(Element(list,1) ));

      -- divide the address and the port
      addr_str := Element(list,2);
      list := Split(To_String(addr_str),':');

      tmp.Address.Addr := Inet_Addr(To_String(Element(list,1)));
      tmp.Address.Port := Port_Type(Natural'Value(To_String(Element(list,2))));

      return tmp;
   end  Value;


      -- return a string from the Peer_DB
   function Peer_DB_To_String(db: Peer_DB) return Unbounded_String is
      peer: DB_Item;
      msg: Unbounded_String ;
   begin

       -- how many peer in the list
      msg := Integer'Image(Server_DB.Count(db)) & To_Unbounded_String(" ");
         -- Create the list of peer as string
      for i in Integer range 1 .. Server_DB.Count(db) loop
         peer := Get(db,i);
         msg := msg & Image(peer);
      end loop;

      return msg;
   end  Peer_DB_To_String;



--     procedure PrintAll(db: in Peer_DB) is
--        tmp: DB_Element_Access;
--     begin
--
--        Put("Count: ");
--        Put(db.num_item);
--        New_Line;
--
--        tmp := db.current;
--
--        -- No items in the list
--        if db.num_item = 0 then
--           raise peer_not_present;
--        end if;
--
--
--
--        while (tmp.next /= null) loop
--           Put(Integer(tmp.data.Address));
--           New_Line;
--           tmp := tmp.next;
--        end loop;
--
--        Put(Integer(tmp.data.Address));
--        New_Line;
--
--     end PrintAll;


end Server_DB;

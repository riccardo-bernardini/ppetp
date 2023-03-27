package body Input.Address_List is

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout(List : in out Peer_Table;
                         How  : in Duration) is
   begin
      List.Timeout := How;
   end Set_Timeout;


   ------------
   -- Insert --
   ------------

   procedure Insert(List : in out Peer_Table;
                    Item : in Inet_Addr_Type) is
      Where : Table_Record_Pt;
   begin

      -- Cerca se è già esistente:
      --    - SI :  cancella il vecchio elemento e inserisce il nuovo in
      --    -	    ultima posizione; non incrementa size
      --    - NO :  inserisce in ultima posizione e incrementa size

      Find(List, Item, Where);

      if Where /= null then
         Add_At_The_End(List, Item);
         Delete(List, Where);
      else
         Add_At_The_End(List, Item);
      end if;

   end Insert;

   ------------------------------
   -- Get_Next_Expiration_Time --
   ------------------------------

   function Get_Next_Expiration_Time(List : in Peer_Table) return Time is
   begin
      if List.First = null then
         raise Empty_List;
      end if;

      return List.First.Item.Expires;
   end Get_Next_Expiration_Time;


   ------------------------------
   -- Get_Next_Expiration_Time --
   ------------------------------


   procedure Get_And_Delete_Expired(List : in out Peer_Table;
                                    Addr : out Inet_Addr_Type) is
   begin

      if clock >= List.First.Item.Expires then
         Addr := List.First.Item.Addr;
      else
         Addr := No_Inet_Addr;
      end if;

      Delete(List, List.First);

   end Get_And_Delete_Expired;

   ----------
   -- Size --
   ----------
   function Size(List : in Peer_Table) return Natural is
   begin
      return List.Size;
   end Size;

   -----------
   -- Clear --
   -----------
   procedure Clear(List : in out Peer_Table) is
      Where : Table_Record_Pt := List.First;
   begin
      while Where /= null loop
         Delete(List, Where);
         Where := List.First;
      end loop;
   end Clear;


   -----------------------------------------------------------------------------
   ------------------------  Only for internal use -----------------------------
   -----------------------------------------------------------------------------

   ----------
   -- Find --
   ----------
   procedure Find(List  : in out Peer_Table;
                  Item  : in  Inet_Addr_Type;
                  Point : out Table_Record_Pt) is
      Tmp : Table_Record_Pt;
   begin

      if List.Size = 0 then
         Point := null;
         return;
      end if;

      Tmp := List.First;

      while Tmp /= null loop
         if Tmp.Item.Addr = Item then
            Point := Tmp;
            return;
         end if;

         Tmp := Tmp.Next;
      end loop;

      Point := null;
   end Find;

   --------------------
   -- Add_At_The_End --
   --------------------
   procedure Add_At_The_End(List  : in out Peer_Table;
                            Item  : in  Inet_Addr_Type) is
      Element : Table_Item;
   begin

      Element.Addr    := Item;
      Element.Expires := Clock + List.Timeout;

      declare
         List_Item : Table_Record_Pt := new Table_Record;
      begin
         List_Item.Item := Element;

         if List.Size = 0 then
            List_Item.Prev := null;
            List_Item.Next := null;

            List.First := List_Item;
            List.Last  := List_Item;
         else
            List_Item.Next := null;
            List_Item.Prev := List.Last;
            List.Last.Next := List_Item;
            List.Last 	   := List_Item;
         end if;
      end;

      List.Size := List.Size + 1;

   end Add_At_The_End;



   ------------
   -- Delete --
   ------------

   procedure Delete(List  : in out Peer_Table;
                    Where : in out Table_Record_Pt) is
   begin


      if Where.Prev = null then
         List.First := Where.Next;
      end if;

      if Where.Next = null then
         List.Last := Where.Prev;
      end if;

      if Where.Prev /= null then
         Where.Prev.Next := Where.Next;
      end if;

      if Where.Next /= null then
         Where.Next.Prev := Where.Prev;
      end if;

      List.Size := List.Size - 1;
      -- TODO --  Free(Where)
      Free(Where);
   end Delete;



end Input.Address_List;

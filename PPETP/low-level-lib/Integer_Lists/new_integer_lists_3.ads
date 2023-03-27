
with Ada.Finalization;		use  Ada.Finalization;

generic
   type Item_Type is mod <>;
package New_Integer_Lists_3 is

   type New_Integer_List_3 is new Controlled with private;
   type New_Integer_List_3_pt is access New_Integer_List_3;

   function "=" (Left, Right : New_Integer_List_3_pt) return Boolean;


   type Duplicate_Policy is (Die, Ignore);




   procedure Insert (List         : in out New_Integer_List_3;
                     Item         : in     Item_Type;
                     On_Duplicate : in     Duplicate_Policy := Die);

   function Contains (List : in New_Integer_List_3;
                      Item : in Item_Type) return Boolean;

   procedure Set_Sup_Limit(List  : in out New_Integer_List_3;
                           Limit : in Item_Type);


   -- for debug only
   procedure Dump(List : in New_Integer_List_3);

private
   type Item_Buffer is array (Item_Type range <>) of Boolean;
   type Item_Buffer_Ref is access Item_Buffer;

   Table_Size: constant Item_Type := 128;


   type New_Integer_List_3 is new Controlled with record
      First     : Item_Type;
      Buffer    : Item_Buffer_Ref;
      --Inf_Limit : Item_Type;
      Sup_Limit : Item_Type;
      Offset    : Item_type;
      Empty	: Boolean := True;
      Entity_ID : Natural;	-- used to "compare" objects
   end record;

   procedure Initialize(Object: in out New_Integer_List_3);
end New_Integer_Lists_3;

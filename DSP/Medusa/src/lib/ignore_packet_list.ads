with Timestamps;
with Integer_Lists;

package Ignore_Packet_List is
   type Ignore_List is private;
   type Ingore_List_Array is array(Natural range <>) of Ignore_List;

   --
   --  Initialize an Ignore_List for streams with N_Components
   --
   procedure Init (List         : in out Ignore_List;
                   N_Components : in     Positive);

   --
   -- Add to List the component number Component of the packet
   -- with timestamp equal to Timestamp
   --
   procedure Ignore_Packet (List      : in out Ignore_List;
                            Timestamp : Multimedia_Timestamp;
                            Component : Component_Index);

   --
   -- Add to List all the components of the packet
   -- with timestamp equal to Timestamp
   --
   procedure Ignore_Packet (List      : in out Ignore_List;
                            Timestamp : in     Multimedia_Timestamp);

   --
   -- Return True if the component number Component of the packet
   -- with timestamp equal to Timestamp is in List
   --
   function Is_To_Be_Ignored (List      : Ignore_List;
                              Timestamp : Multimedia_Timestamp;
                              Component : Component_Index)
                             return Boolean;

   function Is_To_Be_Ignored (List      : Ignore_List;
                              Timestamp : Multimedia_Timestamp)
                             return Boolean;

private
   package Timestamp_Lists is
      new Integer_Lists(Multimedia_Timestamp);

   use type Timestamp_Lists.Integer_List;

   protected type Basic_Ignore_List is
      procedure Insert   (Timestamp : in Multimedia_Timestamp);
      function  Contains (Timestamp : in Multimedia_Timestamp)
                         return Boolean;
   private
      My_List : Timestamp_Lists.Integer_List;
   end Basic_Ignore_List;

   type Basic_Ignore_List_Array is
     array(Natural range <>) of Basic_Ignore_List;

   type Basic_Ignore_List_Array_Pt is
     access Basic_Ignore_List_Array;

   type Ignore_List is record
      Internal_Lists : Basic_Ignore_List_Array_Pt;
   end record;
end Ignore_Packet_List;

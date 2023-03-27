with Ada.Characters.Latin_1;
use  Ada.Characters.Latin_1;

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
use  Ada.Strings;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Maps;
use  Ada.Strings.Maps;

with TCP_Query;

with Length_Delimited_Strings;
use  Length_Delimited_Strings;

with Text_Io;
use  Text_Io;
package body Query_State is
   --====================--
   -- Internal Functions --
   --====================--

   procedure Check_Var_Name (Name : in String) is
   begin
      if (Index(Name, To_Set(' ')) /= 0) then
         raise Malformed_Query;
      end if;
   end Check_Var_Name;

   function Do_Query (Ask_To : in DB_Connection;
                      Query  : in String) return String
   is
   begin
      return TCP_Query.Do_Query(Server => Ask_To.Addr,
                                Query  => Query);
   end Do_Query;

   --====================--
   -- Exported Functions --
   --====================--

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Connection : in out DB_Connection;
      Port       : in     Port_Type;
      Host       : in     String := "127.0.0.1")
   is
   begin
      Connection.Addr := (Family => Family_Inet,
                          Addr   => Inet_Addr (Host),
                          Port   => Port);
   end Connect;


   procedure Connect (Connection : in out DB_Connection;
                      Port       : in     Positive;
                      Host       : in     String := "127.0.0.1")
   is
   begin
      Connect(Connection, Port_Type(Port), Host);
   end Connect;

   ---------
   -- Get --
   ---------


   function Get (Connection : in DB_Connection;
                 Var_Name   : in String) return String
   is
      Idx          : Natural;
      LDS_Set      : LDS_Set_Type;
   begin
      Check_Var_Name(Var_Name);

      declare
         Query_Result : String := Do_Query(Ask_To => Connection,
                                           Query  => "GET " & Var_Name);
      begin
         if (Query_Result(1..3) /= "000") then
            raise Query_Error; -- with Query_Result(1..3);
         end if;

         Idx := Index (Source => Query_Result,
                       Set    => To_Set(LF));

         if (Idx = 0) then
            raise Malformed_Reply;
         end if;

         Load(LDS_Set, Query_Result(Idx+1..Query_Result'Last));

         if (N_Elem(LDS_Set) /= 1) then
            raise Malformed_Reply;
         end if;

         return Element(LDS_Set, 1);
      end;
   end Get;

   function Get_Int (Connection : in DB_Connection;
                     Var_Name   : in String) return Integer
   is
   begin
      return Integer'Value(Get(Connection, Var_Name));
   end Get_Int;


   ---------
   -- Set --
   ---------

   procedure Set (Connection : in DB_Connection;
                  Var_Name   : in String;
                  Var_Value  : in String)
   is
   begin
      Check_Var_Name(Var_Name);

      declare
         Ignored : String := Do_Query(Ask_To => Connection,
                                      Query  => "SET "
                                      & Var_Name
                                      & " " & Encode(Var_Value));
      begin
         null;
      end;
   end Set;

   procedure Set (Connection : in DB_Connection;
                  Var_Name   : in String;
                  Var_Value  : in Integer)
   is
   begin
      Set(Connection, Var_Name, Integer'Image(Var_Value));
   end Set;

end Query_State;

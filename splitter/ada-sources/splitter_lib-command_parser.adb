with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Splitter_Lib.BSD_Sockets; use Splitter_Lib.BSD_Sockets;

with Ada.Text_IO;

package body Splitter_Lib.Command_Parser is
   procedure Parse_Command_Line (Config : out Config_Data) is

      type Param_Name is new String (1 .. 3);

      function To_S (X : Unbounded_String) return String
                     renames To_String;

      function To_U (X : String) return Unbounded_String
                     renames To_Unbounded_String;

      ---------------------
      -- Split_Parameter --
      ---------------------

      procedure Split_Parameter (Param : in     String;
                                 Name  :    out Unbounded_String;
                                 Value :    out Unbounded_String)
      is
         Idx : Natural;
      begin
         Idx := Index (Source  => Param,
                       Pattern => "=");

         if (Idx = 0) then
            Name  := To_U (Param);
            Value := Null_Unbounded_String;
         else
            Name  := To_U (Param (Param'First .. Idx - 1));
            Value := To_U (Param (Idx + 1 .. Param'Last));
         end if;
      end Split_Parameter;

      ----------------
      -- To_Natural --
      ----------------

      function To_Natural (X : Unbounded_String)
                           return Natural is
      begin
         if X = Null_Unbounded_String then
            raise Bad_Command;
         end if;

         return Natural'Value (To_S (X));
      end To_Natural;

      --------------
      -- To_Float --
      --------------

      function To_Float (X : Unbounded_String)
                         return Float is
      begin
         if X = Null_Unbounded_String then
            raise Bad_Command;
         end if;

         return Float'Value (To_S (X));
      end To_Float;

      function To_Target (X : Unbounded_String)
                          return Outbound_Connection is
         Comma_Pos : Natural;
         Colon_Pos : Natural;
         Result    : Outbound_Connection;

         use BSD_Sockets;
      begin
         Comma_Pos := Index (Source  => X,
                             Pattern => ",",
                             From    => 1);
         if Comma_Pos < 1 then
            raise Bad_Command;
         end if;

         Colon_Pos := Index (Source  => X,
                             Pattern => ":",
                             From    => Comma_Pos + 1);

         if Colon_Pos = 0 then
            raise Bad_Command;
         end if;

         Result := (Internal_Port =>
                      To_Natural (Unbounded_Slice (X, 1, Comma_Pos - 1)),
                    Remote_Host   =>
                      (Family => Family_Inet,
                       Addr   => Inet_Addr (Slice (X, Comma_Pos + 1, Colon_Pos - 1)),
                       Port   =>
                         Port_Type (To_Natural
                           (Unbounded_Slice (X, Colon_Pos + 1, Length (X))))));

         return Result;
      end To_Target;

   begin

      if (Argument_Count < 2) then
         raise Bad_Command;
      end if;

      Config := (Source_Port  => 0,
                 Control_Port => 0,
                 Ext_Address  => BSD_Sockets.any_Inet_Addr,
                 Timeout      => 5.0,
                 Ignored_SSRC => 0,
                 Target       => No_Connection);

      declare
         Name  : Unbounded_String;
         Value : Unbounded_String;
      begin
         for I in 1..Argument_Count loop
            Split_Parameter (Argument (I), Name, Value);

            declare
               N : String := To_S (Name);
            begin

               --Ada.Text_IO.Put_Line("*******  '" & N & "'  *********");

               if N = "src" then
                  Config.Source_Port := To_Natural (Value);
               elsif N = "ctl" then
                  Config.Control_Port := To_Natural (Value);
               elsif N = "address" then
                  Ada.Text_IO.Put_Line("Ada: Address: " &To_S(Value));
                  Config.Ext_Address := bsd_sockets.Any_Inet_Addr; -- Inet_Addr(To_S(Value));
               elsif N = "timeout" then
                  Config.Timeout := Duration(To_Float (Value));
                  --Ada.Text_IO.Put_Line("*****!!!!! " & Config.Timeout'img & "   !!!!*******");
               elsif N = "ignore" then
                  Config.Ignored_SSRC := RTP.SSRC_Type (To_Natural (Value));
               elsif N = "target" then
                  Config.Target := To_Target (Value);
               else
                  raise Bad_Command;
               end if;
            end;
         end loop;
      end;


      if Config.Source_Port = 0 or Config.Control_Port = 0
      -- or  Config.Ext_Address = No_Inet_Addr
      then
         raise Bad_Command;
      end if;
   end Parse_Command_Line;
end Splitter_Lib.Command_Parser;

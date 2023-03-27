with Input_Sources.Strings;   use Input_Sources.Strings;
with Sax.Readers;             use Sax.Readers;
with DOM.Readers;             use DOM.Readers;
with DOM.Core;                use DOM.Core;
with DOM.Core.Documents;      use DOM.Core.Documents;
with DOM.Core.Nodes;          use DOM.Core.Nodes;
with DOM.Core.Attrs;          use DOM.Core.Attrs;
with DOM.Core.Elements;       use DOM.Core;
with Unicode.Ces.Utf8;        use Unicode.Ces.Utf8;
with Schema.Dom_Readers;      use Schema.Dom_Readers;
with Str_Conversion;          use Str_Conversion;
with Ada.Streams;             use Ada.Streams;
with Profiles.Config;         use Profiles.Config;
with Network;                 use Network;
with Network_Utilities;       use Network_Utilities;
with Byte_Arrays;             use Byte_Arrays;
with Ada.Exceptions;
with Schema.Validators;
--with Tokenize;		      use Tokenize;
with Profiles;		      use Profiles;


with Ada.Text_IO; use Ada.Text_IO;

package body Configuration.XML is
   Invalid_Auth_Payload       : exception;
   Invalid_Auth_Profile       : exception;
   Invalid_Processing_Profile : exception;
   Invalid_Port               : exception;
   Invalid_Channel_ID         : exception;
   Invalid_Address_Type       : exception;

   Profile_Param_Table  : Config_Table;
   Session_Profile      : Profile_Type;

   function To_Integer is
      new String_To_Integer (Integer);

   ----------------
   -- Attr_Value --
   ----------------

   function Attr_Value (X         : Node;
                        Attr_Name : String) return String is
      A : Attr := Get_Named_Item(Attributes(X), Attr_Name);
   begin
      if (A /= null) then
         return Value (A);
      else
         return "null";
      end if;
   end Attr_Value;

   ------------
   -- To_Int --
   ------------

   function To_Int (X : Attr) return Integer is
   begin
      return To_Integer (Value (X), 10);
   end To_Int;

   -----------------
   -- To_StreamID --
   -----------------
   function To_StreamID(X : Attr) return Stream_ID is
   begin
      return Stream_ID'Value(Value(X));
   end To_StreamID;


   ----------
   -- Path --
   ----------

   function Path (X : Node) return String is

      function Param (X : Node) return String is
         function Get_Param (X : Node) return String is
            Name : String := Node_Name(X);
         begin
            if (Name = "session") then
               return Attr_Value (X, "port");
            elsif (Name = "peer") then
               return  "";
            elsif (Name = "channel") then
               return Attr_Value (X, "id");
            elsif (Name = "address") then
               return "addr=" & Attr_Value (X, "address")
                 & ":"   & Attr_Value (X, "port")
                 & ", ch=" & Attr_Value (X, "channel");
            elsif (Name = "auth-data") then
               return "command=" & Attr_Value (X, "command")
                 & ", profile=" & Attr_Value (X, "profile-name");
            else
               return "";
            end if;
         end Get_Param;

         Result : String := Get_Param(X);
      begin
         if (Result = "") then
            return "";
         else
            return "(" & Result & ")";
         end if;
      end Param;
      Name : String := Node_Name (X);
   begin
      if (Name = "configuration") then
         return "";
      end if;

      declare
         Parent : Node := Parent_Node (X);
         Result : String := Path (Parent) & "/" & Name & Param (X);
      begin
         return Result;
      end;
   end Path;


   function Get_Processing_Profile (X : DOM.Core.Element) return Profiles.Profile_Type is
	Prof : Attr;
   begin
      Prof := Get_Named_Item (Attributes (X), "name");
      if (Prof /= null) then
         declare
            Name : String := Value (Prof);
         begin
            return Profiles.Profile_Type'Value (Name & "_profile");
         exception
            when Constraint_Error =>
               raise Invalid_Processing_Profile
                 with "name=" & Name & "in" & Path (X);
         end;
      else
         Prof := Get_Named_Item (Attributes (X), "id");
         pragma Assert (Prof /= null);

         declare
            ID : String := Value (Prof);
         begin
            return Profiles.Profile_Type'Val (To_Integer(ID, 10));
         exception
            when Constraint_Error =>
               raise Invalid_Processing_Profile
                 with "ID=" & ID & "in" & Path (X);
         end;
      end if;
   end Get_Processing_Profile;

   -------------
   -- To_Port --
   -------------

   function To_Port (X : Attr) return Port_Type is
   begin
      return Port_Type (To_Integer (Value (X), 10));
   exception
      when Invalid_Number =>
         raise Invalid_Port;
   end To_Port;


   ------------------------
   -- Parse_Auth_Profile --
   ------------------------

   function Parse_Auth_Profile (X : DOM.Core.Element) return Auth_Profile is
      Prof : Attr;
   begin
      pragma Assert (Node_Name (X) = "auth-profile");
      -- Dump (X);
      Prof := Get_Named_Item (Attributes (X), "name");

      if (Prof /= null) then
         -- Put_Line ("get_auth1:'" & Value(Prof) &"'");
         return Auth_Profile'Value (Value (Prof) & "_profile");
      else
         Prof := Get_Named_Item (Attributes (X), "id");
         pragma Assert (Prof /= null);
         -- Put_Line ("get_auth2:" & Value(X));
         return Auth_Profile'Val (To_Int (Prof));
      end if;
   exception
      when Constraint_Error =>
         raise Invalid_Auth_Profile;
   end Parse_Auth_Profile;

   ---------------------
   -- To_Auth_Payload --
   ---------------------

   function Get_Auth_Payload (Root : DOM.Core.Element) return Byte_Array is
      -- Convert an hexadecimal string to an Auth_Payload
      function To_Auth_Payload (Data : String) return Byte_Array is
         function Str_To_Element is
           new String_To_Modular (Stream_Element);

      begin
         -- Since each byte is represented by two hexadecimal values,
         -- the length of the input string must be even
         if (Data'Length mod 2 /= 0) then
            raise Invalid_Auth_Payload;
         end if;

         -- Put_Line("[" & Data & "]");

         return Result : Byte_Array (1 .. Data'Length / 2) do
            declare
               Idx : Positive := Data'First;
            begin
               for I in Result'Range loop
                  Result (I) := Str_To_Element (Data (Idx .. Idx + 1), 16);
                  Idx := Idx + 2;
               end loop;
            end;

            -- Dump (Result, "auth_payload");
         end return;
      end To_Auth_Payload;

      Value_Attr : Attr := Get_Named_Item(Attributes(Root), "value");
   begin
      pragma Assert (Node_Name (Root) = "auth-token");
      pragma Assert (Value_Attr /= null);

      return To_Auth_Payload (Value (Value_Attr));
   end Get_Auth_Payload;


--     ---------------------
--     -- Get_Channel_IDs --
--     ---------------------
--     function Get_Channel_IDs(Root: DOM.Core.Element) return ChannelID_Array_Pt is
--        --Channels_Att : Attr := Get_Named_Item (Attributes (Root), "channel");
--        Channels_List : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "channel");
--
--        Result        : ChannelID_Array_Pt;
--  --      Ch_List       : Token_List;
--     begin
--
--        pragma Assert (Channels_List /= null);
--
--        -- Comma separated Channel_ID list string's
--  --      Ch_List := Split(Value(Channels_Att), ',');
--
--        Result := new ChannelID_Array(1..length(Channels_List));
--
--
--        Item (Channels_List, Idx-1)
--
--        for Idx in 1..Length(Channels_List) loop
--           Result(Idx) :=
--        end loop;
--
--
--        for i in 1.. length(Ch_List) loop
--           Result(i) := Channel_ID'Value( To_String(Tokenize.Element(Ch_List,i)) );
--        end loop;
--
--
--        return Result;
--     end Get_Channel_IDs;


   -------------------
   -- Parse_Profile --
   -------------------

   -- Root must be a node of type "profile".  This procedure
   -- parse the "profile" node (which contains informations about
   -- the processing profile and the parameters to be used) and
   -- returns the the result in Parameters.
   procedure Parse_Profile (Root       : in     DOM.Core.Element;
                            Profile    :    out Profiles.Profile_Type;
                            Parameters :    out Parameters_Class_Pt) is
      Prof_Name : Attr := Get_Named_Item (Attributes (Root), "name");
      Params : Node_List := Elements.Get_Elements_By_Tag_Name (Root, "parameter");

      N      : Node;
   begin
      pragma Assert (Node_Name (Root) = "profile");

      Profile := Get_Processing_Profile (Root);

    --  Put_Line("Get_Processing_Profile: OK");

      -- The interpretation of pairs parameter name/value depends
      -- on the specific profile.  Therefore, we first  collect the
      -- pairs and successively we give them to the specific
      -- config_parser associated with the processing profile.


      -- Step 1: Get the (name, value) pairs

      for I in 1 .. Length (Params) loop




         N := Item (Params, I - 1);

--           Put_Line("Param " & Value(Get_Named_Item (Attributes (N), "name") ) & " = " &
--               Value(Get_Named_Item (Attributes (N), "name") ));

         Insert (Table => Profile_Param_Table,
                 Name  => Value (Get_Named_Item (Attributes (N), "name")),
                 Value  => Value (Get_Named_Item (Attributes (N), "value")));
      end loop;



      -- Step 2: Give the pairs to the profile Config_Parser
      declare
         Parser : Config_Class_Pt := New_Config_Parser (Profile);
         Errors : Config_Error_List;
      begin


         Parser.Parse (Table  => Profile_Param_Table,
                       Result => Parameters,
                       Errors => Errors);


      end;

   end Parse_Profile;


   ----------------------
   -- Parse_Auth_Token --
   ----------------------

   function Parse_Auth_Token (Profile : Auth_Profile;
                              Root    : DOM.Core.Element) return Command_Auth_Data is
      Command_Attr : Attr := Get_Named_Item (Attributes (Root), "command");
      This_Command : Command_Type;
   begin
      pragma Assert (Node_Name (Root) = "auth-token");

      if (Command_Attr = null) then
         This_Command := Any;
      else
         This_Command := Command_Type'Value (Value (Command_Attr));
      end if;

      return (Command     => This_Command,
              Credentials => New_Credential (Profile => Profile,
                                             Data    => Get_Auth_Payload (Root)));
   end Parse_Auth_Token;

   ----------------
   -- Parse_Auth --
   ----------------

   function Get_Auth_Data (Root : DOM.Core.Element) return Auth_Array_Pt is
      Auth_Data_List : Node_List :=
                         Elements.Get_Elements_By_Tag_Name (Root, "auth-data");
   begin
      if (Length (Auth_Data_List) = 0) then
         return null;
      else
         pragma Assert (Length (Auth_Data_List) = 1);

         declare
            Child : DOM.Core.Element := Item (Auth_Data_List, 0);
            Profile_El   : Node_List :=
                             Elements.Get_Elements_By_Tag_Name (Child,
                                                                "auth-profile");
            Token_Nodes  : Node_List :=
                             Elements.Get_Elements_By_Tag_Name (Child,
                                                                "auth-token");
            Result       : Auth_Array_Pt :=
                             new Auth_Array (1 .. Length (Token_Nodes));
            This_Profile : Auth_Profile;
         begin
            pragma Assert (Node_Name (Child) = "auth-data");
            pragma Assert (Length (Profile_El) = 1);

            This_Profile := Parse_Auth_Profile (Item (Profile_El, 0));

            for I in Result'Range loop
               Result (I) := Parse_Auth_Token
                 (This_Profile, Item (Token_Nodes, I - Result'First));
            end loop;

            return Result;
         exception
            when Invalid_Auth_Payload =>
               raise Invalid_Auth_Payload with Path (Child);
         end;
      end if;
   end Get_Auth_Data;

   -------------------
   -- Parse_Address --
   -------------------

   procedure Parse_Address (Root     : in     DOM.Core.Element;
                            Address  :    out Inet_Addr_Type;
                            Port     :    out Port_Type) is

      Addr_Attr    : Attr := Get_Named_Item (Attributes (Root), "address");
      Port_Attr    : Attr := Get_Named_Item (Attributes (Root), "port");

   begin
      pragma Assert (Node_Name (Root) = "address");
      pragma Assert (Addr_Attr/= null);
      pragma Assert (Port_Attr /= null);


      Address   := Inet_Addr (Value (Addr_Attr));
      Port      := To_Port (Port_Attr);
   end Parse_Address;




   -------------------
   -- Parse_Channel --
   -------------------

   function Parse_Channel (Root : DOM.Core.Element) return Channel_Config is
      Id         : Attr := Get_Named_Item (Attributes (Root), "id");
      Params     : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "parameter");

      Parameters : Parameters_Class_Pt;
      Id_Number  : Integer;
   begin
      pragma Assert (Node_Name(Root) = "channel");

      Id_Number := To_Integer (Value (Id), 10);

      --Put_Line("#### Parse_Channel ID: " & Id_number'img);

      -- Elaborate parameters if any
      if Length (Params) /= 0 then
         declare
            Parser : Config_Class_Pt := New_Config_Parser (Session_Profile);
            Errors : Config_Error_List;
            N      : Node;
         begin

            for I in 1 .. Length (Params) loop
               N := Item (Params, I - 1);

               Insert (Table => Profile_Param_Table,
                       Name  => Value (Get_Named_Item (Attributes (N), "name")),
                       Value => Value (Get_Named_Item (Attributes (N), "value")));
            end loop;


            Parser.Parse (Table  => Profile_Param_Table,
                          Result => Parameters,
                          Errors => Errors);

            -- remove the parameters inserted because is unique for a channel
            for I in 1 .. Length (Params) loop
               N := Item (Params, I - 1);

               Remove (Table => Profile_Param_Table,
                       Name  => Value (Get_Named_Item (Attributes (N), "name")));
            end loop;
         end;
      else
         -- if the tables is not empty use the same parameters of the session
         declare
            Parser : Config_Class_Pt := New_Config_Parser (Session_Profile);
            Errors : Config_Error_List;
         begin
            if Size(Profile_Param_Table) /= 0 then
               Parser.Parse (Table  => Profile_Param_Table,
                             Result => Parameters,
                             Errors => Errors);
            end if;
         end;
      end if;

      return (Id         => Channel_Id(Id_Number),
              Profile    => Session_Profile,
              Parameters => Parameters,
              Auth       => Get_Auth_Data (Root));
   end Parse_Channel;


   ----------------
   -- Parse_Peer --
   ----------------
   function Parse_Peer (Root : DOM.Core.Element) return Input_Config is
      Id_Att        : Attr := Get_Named_Item (Attributes (Root), "id");
      Addr_Type_Att : Attr := Get_Named_Item (Attributes (Root), "address-type");
      Addr          : Node_List := Elements.Get_Elements_By_Tag_Name (Root, "address");
      Ch_Node       : Node_List := Elements.Get_Elements_By_Tag_Name (Root, "channel");


      Addr_Type     : Address_Type;


      Address   : Inet_Addr_Type;
      Port      : Port_Type;

      Ch: Channel_Config_Array_Pt := new Output_Conf_Array(1.. Length(Ch_Node));

   begin
      pragma Assert (Node_Name (Root) = "peer");
      pragma Assert (Length (Addr) = 1);


--      Put_Line("Parse Peer");

      if Value(Addr_Type_Att) = "ip" then
         Addr_Type := ip_type;
      elsif Value(Addr_Type_Att) = "ice" then
         Addr_Type := ice_type;
      else
         raise Invalid_Address_Type with
           "address-type not valid: " & Value(Addr_Type_Att);
      end if;


--      Put_Line("Parse Address");


      --*TODO possono esserci più address
      Parse_Address (Root      => Item (Addr, 0),
                     Address   => Address,
                     Port      => Port);

--      Put_Line("Parse Address OK");

--      Put_Line("Parse Channels: "  & Length(Ch_Node)'img);

      for i in 1 .. Length(Ch_Node) loop
         Ch(i) := Parse_Channel( Item (Ch_Node, I-1));
      end loop;


--      Put_Line("Parse Channels: ok");




      return (Id        => Value( Value(Id_Att) ),
              Addr_Type => Addr_Type,
              Addr      => Address,
              Port      => Port,
              Channels  => Ch,
              Auth      => Get_Auth_Data (Root));
   end Parse_Peer;




   ------------------
   -- Parse_Server --
   ------------------
   function Parse_Server(Root : DOM.Core.Element) return Server_Config_Pt is
        Addr_Attr : Attr;
        Port_Attr : Attr;
        Id_Attr   : Attr;



      Address : Inet_Addr_Type;
      Port    : Port_Type;
      Id      : Integer;
   begin
      pragma Assert (Node_Name (Root) = "server");

      Addr_Attr := Get_Named_Item (Attributes (Root), "address");
      Port_Attr := Get_Named_Item (Attributes (Root), "port");
      Id_Attr   := Get_Named_Item (Attributes (Root), "id");

      Address := Inet_Addr (Value (Addr_Attr));
      Port    := To_Port (Port_Attr);
      Id      := To_Integer(Value (Id_Attr), 10);


      return new Server_Config'(Addr => Address,
                                Port => Port,
                                Id   => Id);

   end Parse_Server;


   -----------------
   -- Parse_Input --
   -----------------
   function Parse_Input (Root : DOM.Core.Element) return Input_Config_Array_Pt is
      List_Input  : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "peer");

      Peers : Input_Config_Array_Pt;
   begin
      pragma Assert (Node_Name (Root) = "input");

      Put_Line("Number of Input: " & Length(List_Input)'img);
      if Length(List_Input) = 0 then
         return null;
      else

         Peers := new Input_Conf_Array(1..Length(List_Input));


         for Idx in 1 .. Length (List_Input) loop
            Peers(Idx) := Parse_Peer (Item (List_Input, Idx-1));

--              for k in 1 .. Peers(Idx).Channels'length loop
--              	Put_Line("#### " & Idx'img & " -->  Ch: " & Peers(Idx).Channels(k).Id'img);
--              end loop;
         end loop;

         return Peers;
      end if;
   end Parse_Input;


   -----------------
   -- Parse_Output --
   -----------------
   function Parse_Output (Root : DOM.Core.Element) return Channel_Config_Array_Pt is
      List_Output : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "channel");

      Channels : Channel_Config_Array_Pt;
   begin
      pragma Assert (Node_Name (Root) = "output");

   --   Put_Line("Number of Output: " & Length(List_Output)'img);
      if Length(List_Output) = 0 then
         return null;
      else
         Channels := new Output_Conf_Array(1..Length(List_Output));

         for Idx in 1..Length(List_Output) loop
            Channels(Idx) := Parse_Channel (Item (List_Output, Idx-1));
         end loop;

         return Channels;
      end if;

   end Parse_Output;


   -------------------
   -- Parse_Session --
   -------------------

   function Parse_Session (Root : DOM.Core.Element) return Session_Config_Pt is
      Input  : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "input");
      Output : Node_List := Elements.Get_Elements_By_Tag_Name(Root, "output");
      StreamID_Attr: Attr;
      Result : Session_Config_Pt :=
        new Session_Config;--(N_Inputs  => Length (List_Input),
                           --N_Outputs => Length (List_Output));


      -- The server address and id
      Server : Node_List := Elements.Get_Elements_By_Tag_Name (Root, "server");

      Child   : Node_List;
      Profile : Profiles.Profile_Type;
      Default_StreamID : Stream_ID;
      Parameters : Parameters_Class_Pt;

   begin
      pragma Assert (Node_Name(Root) = "session");

      -- Each session node has children of type "peer" and type "channel"

      --Put_Line("Parse session");

      if length(Server) /= 0 then
         Result.Server  := Parse_Server(Item(Server,0));
      end if;


      StreamID_Attr := Get_Named_Item(Attributes(Root), "streamID");
      Default_StreamID := To_StreamID(StreamID_Attr);

     -- Put_Line("scan for profile");
      Child := Elements.Get_Elements_By_Tag_Name (Root, "profile");

      --Put_Line("parse profile");
      Parse_Profile(Root       => Item (Child, 0),
                    Profile    => Profile,
                    Parameters => Parameters);

      Result.Profile  := Profile;
      Result.Default_StreamID := Default_StreamID;
      Result.Parameters := Parameters;
      Session_Profile := Profile;

      --Put_line("parse profile OK");



--        for Idx in 1 .. Length (List_Input) loop
--           Result.Inputs (Idx) := Parse_Peer (Item (List_Input, Idx-1));
--        end loop;

      Put_Line("Parse Input");
      Result.Inputs := Parse_Input(Item(Input,0));

      Put_Line("Parse Input OK");

--        for Idx in 1..Length(List_Output) loop
--           Result.Outputs (Idx) := Parse_Channel (Item (List_Output, Idx-1));
--        end loop;

      Put_Line("Parse outout OK");
      Result.Outputs := Parse_Output(Item(Output,0));
      Put_Line("Parse Output OK");

      return Result;
   end Parse_Session;

   -----------
   -- Parse --
   -----------

   function Parse (Str : String) return Config_Data is
      Input  : String_Input;
      Reader : Schema.Dom_Readers.Tree_Reader;
      Doc    : Document;
      List   : Node_List;

      use Ada.Exceptions;
   begin



      -- Convert the XML source to internal format (Document)
      Open (Input => Input, Str => Str, Encoding => Utf8_Encoding);

      Set_Validating_Grammar (Reader  => Reader,
                              Grammar => Body_Grammar.Grammar);

      Set_Feature (Parser => Reader,
                   Name   => Schema_Validation_Feature,
                   Value  => True);

      Parse (Reader, Input);

      Doc := Get_Tree(Reader);

      -- The root node can have an arbitrary number of
      -- "session" child.  Each child is an entry of
      -- Result.
      List := Get_Elements_By_Tag_Name (Doc, "session");

      return Result : Config_Data (1..Length(List)) do
         for Index in 1 .. Length (List) loop
           Result (Index) := Parse_Session (Item (List, Index-1));
        end loop;
      end return;
   exception
      when E : Schema.VALIDATORS.XML_VALIDATION_ERROR =>
         raise Parse_Error with
         "000 Invalid XML" & Exception_Message (E);
      when E : Invalid_Auth_Payload =>
         raise Parse_Error with
         "001 Invalid authentication data in node " & Exception_Message (E);
      when E : Invalid_Processing_Profile =>
         raise Parse_Error with
         "002 Unknown processing profile " & Exception_Message (E);
      when E : Invalid_Auth_Profile =>
         raise Parse_Error with
         "003 Unknown authentication profile " & Exception_Message (E);
   end Parse;

end Configuration.XML;

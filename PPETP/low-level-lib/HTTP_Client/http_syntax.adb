with Ada.Characters.Handling;                 use Ada.Characters.Handling;
with Text_Io; use Text_Io;

package body HTTP_Syntax is

   -----------------------
   -- Parse_HTTP_Status --
   -----------------------

   function Parse_HTTP_Status (Status_Line : String) return HTTP_Status is
      Protocol : Vstring_Var;
      Code     : Vstring_Var;
      Message  : Vstring_Var;
      Result   : HTTP_Status;

      Status_Code    : constant Pattern := Digit & Digit & Digit;
      Status_Pattern : constant Pattern := Pos(0) & (HTTP_Version * Protocol) &
      Span (" ") & (Status_Code * Code) & Span (" ") & (Rest * Message) & Rpos(0);

      function To_Code_Category (Code : Unbounded_String)
                                 return HTTP_Code_Category
      is
        subtype HTTP_Category_Char is Character range '1'..'5';

      begin
         if (not (Element (Code, 1) in HTTP_Category_Char)) then
            return Extension;
         else
            case HTTP_Category_Char (Element (Code, 1)) is
               when '1' =>
                  return Informational;
               when '2' =>
                  return Success;
               when '3' =>
                  return Redirection;
               when '4' =>
                  return Client_Error;
               when '5' =>
                  return Server_Error;
            end case;
         end if;
      end To_Code_Category;
   begin
      if Match (Subject => Status_Line, Pat => Status_Pattern) then
         Result := (Code     => HTTP_Code'Value (To_String (Code)),
                    Message  => Message,
                    Category => To_Code_Category(Code));
      else
         raise Malformed_Status_Line;
      end if;

      return Result;
   end Parse_HTTP_Status;

   procedure Parse_HTTP_Header (Line           : in     Unbounded_String;
                                Header_Name    :    out Unbounded_String;
                                Header_Value   :    out Unbounded_String;
                                Name_Lowercase : in     Boolean := True)
   is
      Name          : Unbounded_String;
      Value         : Unbounded_String;

      Field_Set     : constant Character_Set :=
                        Text_Set or To_Set (Latin_1.HT);

      Field_Content : constant Pattern := Any(Field_Set);
      Msg_Header    : constant Pattern := Pos (0) & (Token * Name)
        & ":"
        & Nspan(" " & Latin_1.HT)
        & (Arbno (Field_Content) * Value)
        & Nspan (" " & Latin_1.HT)
        & Rpos (0);
   begin
      if (Match (Subject => Line, Pat => Msg_Header)) then
         Header_Value   := Value;
         if (Name_Lowercase) then
            Header_Name := To_Unbounded_String (To_Lower (To_String (Name)));
         else
            Header_Name := Name;
         end if;
      else
         raise Malformed_Header;
      end if;
   end Parse_HTTP_Header;

   procedure Parse_Chunk_Size (Line      : in     Unbounded_String;
                               Size      :    out Natural;
                               Extension :    out Unbounded_String)
   is
      Chunk_Size : Pattern := HEX & Arbno (HEX);
      Chunk_Ext  : Pattern := Arbno (";" & Token & "=" & (Token or Quoted_String));
      Size_Val   : Vstring;
   begin
      if (not Match (Line,
                     Pos (0)                   &
                     (Chunk_Size ** Size_Val)  &
                     (Chunk_Ext  ** Extension) &
                     Rpos (0))) then
         raise Program_Error;
      else
         Size := Natural'Value("16#" & To_String(Size_Val) & "#");
      end if;
      Extension := To_Unbounded_String("");
   end Parse_Chunk_Size;
begin
   Comment := "(" & Arbno (Ctext or Quoted_Pair or (+Comment)) & ")";

end HTTP_Syntax;

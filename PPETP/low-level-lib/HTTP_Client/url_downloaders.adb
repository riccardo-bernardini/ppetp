with GNAT.Sockets;                          use GNAT.Sockets;
with Ada.Characters.Latin_1;                use Ada.Characters;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

use Ada.Strings;

with URL_Parsers;                           use URL_Parsers;
with HTTP_Syntax;                           use HTTP_Syntax;
with GNAT.Spitbol.Patterns;                 use GNAT.Spitbol.Patterns;
with Push_Back_Socket_Streams;              use Push_Back_Socket_Streams;

with Text_Io;                               use Text_Io;

package body URL_Downloaders is

   CRLF : constant String := Latin_1.CR & Latin_1.LF;







   ------------------
   -- Send_Request --
   ------------------

   procedure   Send_Request (HTTP_Stream    : in out Push_Back_Stream;
                             URL_Components : in     URL_Info;
                             Headers        : in     Header_Array)
   is
      Path : Unbounded_String := URL_Components.Path;
      Host : Unbounded_String := URL_Components.Host;
   begin
      -- Print (URL_Components);

      if (URL_Components.Query /= Void_Field) then
         Path := Path & "?" & URL_Components.Query;
      end if;

      if (URL_Components.Fragment /= Void_Field) then
         Path := Path & "#" & URL_Components.Fragment;
      end if;

      if (URL_Components.Port /= URL_Parsers.No_Port) then
         Host := Host & ":"
           & Trim (To_Unbounded_String(URL_Parsers.Port_Number'Image (URL_Components.Port)), Both);
      end if;

      Write (HTTP_Stream, "GET "
             & To_String (Path)
             & " HTTP/1.1"
             & CRLF);

      Write (HTTP_Stream, "Host: "
             & To_String (Host)
             & CRLF);

      for I in Headers'Range loop
         Write (HTTP_Stream, To_String (Headers (I)) & CRLF);
      end loop;

      Write (HTTP_Stream, CRLF);
   end Send_Request;

   ---------------------
   -- Get_Status_Line --
   ---------------------

   procedure Get_Status_Line (From   : in out Push_Back_Stream;
                              Status :    out HTTP_Status) is
      Line        : Unbounded_String;
   begin
      Read_CRLF_Terminated_Line (From, Line);
      Status := Parse_HTTP_Status(To_String(Line));
   end Get_Status_Line;

   procedure Get_Header (HTTP_Stream    : in out Push_Back_Stream;
                         Header_Name    :    out Unbounded_String;
                         Header_Value   :    out Unbounded_String;
                         End_Of_Headers :    out Boolean)
   is
      Line          : Unbounded_String;
   begin
      Read_CRLF_Terminated_Line (HTTP_Stream, Line);
      if (To_String (Line) = "") then
         End_Of_Headers := True;
      else
         Parse_HTTP_Header (Line, Header_Name, Header_Value);
         End_Of_Headers := False;
      end if;
   end Get_Header;

   procedure Get_Chunked_Content (From    : in out Push_Back_Stream;
                                  Content :    out Unbounded_String)
   is
      Line      : Unbounded_String;
      Size      : Natural;
      Extension : Unbounded_String;
      Junk      : Character;
   begin
      Content := Null_Unbounded_String;

      loop
         Read_CRLF_Terminated_Line (From, Line);
         Parse_Chunk_Size (Line, Size, Extension);

         exit when Size = 0;

         Append_String (Src => From, Len => Size, Dst => Content);

         -- Remove the CRLF following the chunk
         Next_Char (From, Junk);
         Next_Char (From, Junk);
      end loop;
   end Get_Chunked_Content;

   ---------
   -- Get --
   ---------

   procedure Get
     (Downloader : in out URL_Downloader;
      URL        : String) is
      Empty_Header_Array : Header_Array(1..0);
   begin
      Get(Downloader, URL, Empty_Header_Array);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Downloader : in out URL_Downloader;
      URL        : String;
      Headers    : Header_Array)
   is
      URL_Components : URL_Info := URL_Parsers.Parse(URL);
   begin
      Put_Line (URL); Print (URL_Components);
      if (To_String(URL_Components.Scheme) /= "http") then
         raise Unimplemented_Scheme;
      end if;

      if (URL_Components.Port = URL_Parsers.No_Port) then
         URL_Components.Port := 80;
      end if;

      declare
         HTTP_Stream    : Push_Back_Stream;
         Content_Len    : Integer := -1;
         End_Of_Headers : Boolean;
         Header_Name    : Unbounded_String;
         Header_Value   : Unbounded_String;
         Host_Info      : Host_Entry_Type :=
                            Get_Host_By_Name
                              (To_String (URL_Components.Host));
      begin
         HTTP_Stream := Make_Push_Back_Stream
           (Addr   => Addresses(Host_Info, 1),
            Port   => Port_Type (URL_Components.Port));

         Send_Request(HTTP_Stream, URL_Components, Headers);

         Get_Status_Line (HTTP_Stream, Downloader.Status);

         Downloader.Headers.Clear;

         loop
            Get_Header (HTTP_Stream, Header_Name, Header_Value, End_Of_Headers);
            exit when End_Of_Headers;

            declare
               Name : String := To_String (Header_Name);
            begin
               if (Downloader.Headers.Contains (Name)) then
                  Header_Value := Header_Value
                    & "," & Downloader.Headers.Element (Name);
               end if;

               Downloader.Headers.Include (Key      => Name,
                                           New_Item => To_String (Header_Value));

               if (Name = "content-length") then
                  Content_Len := Integer'Value (To_String (Header_Value));
               end if;
            end;
         end loop;

         if (Exist_Header (Downloader, "transfer-encoding") and then
               Header (Downloader, "transfer-encoding") = "chunked") then
            Get_Chunked_Content(HTTP_Stream, Downloader.Content);
         elsif (Content_Len > 0) then
            -- Put_Line ("Slurpo " & Integer'Image(Content_Len));
            declare
               Buffer : String (1 .. Content_Len);
            begin
               Get_String(HTTP_Stream, Buffer);
               Downloader.Content := To_Unbounded_String(Buffer);
            end;
         end if;
      end;
   end Get;

   -------------
   -- Content --
   -------------

   function Content
     (Downloader : URL_Downloader)
      return String
   is
   begin
      return To_String(Downloader.Content);
   end Content;

   ------------
   -- Header --
   ------------

   function Header
     (Downloader : URL_Downloader;
      Name       : String)
      return String
   is
      use Header_Maps;
   begin
      return Element (Downloader.Headers, Name);
   exception
      when Constraint_Error =>
         raise No_Such_Header;
   end Header;

   ------------------
   -- Exist_Header --
   ------------------

   function Exist_Header
     (Downloader : URL_Downloader;
      Name       : String)
      return Boolean
   is
   begin
      return Header_Maps.Contains(Downloader.Headers, Name);
   end Exist_Header;

end URL_Downloaders;

--        Fixed.Find_Token (Source     => Status_Line,
--                          Set        => Maps.To_Set (' '),
--                          Test       => Outside,
--                          First      => Protocol.First,
--                          Last       => Protocol.Last);
--
--        if (Token(Status_Line, Protocol) /= "HTTP/1.1") then
--           raise Program_Error;
--        end if;
--
--        Fixed.Find_Token (Source     => Status_Line (Protocol.Last + 1 .. Status_Line'Last),
--                          Set        => Maps.Constants.Decimal_Digit_Set,
--                          Test       => Inside,
--                          First      => Code.First,
--                          Last       => Code.Last);
--
--        Result.Code := HTTP_Code'Value (Token (Status_Line, Code));
--        Result.Message := To_Unbounded_String
--          (Status_Line (Code.Last + 1 .. Status_Line'Last));

--     type URL_Description is
--        record
--           Method     : Unbounded_String;
--           Port       : Port_Type;
--           Host       : Unbounded_String;
--           Username   : Unbounded_String;
--           Password   : Unbounded_String;
--           Filename   : Unbounded_String;
--           Parameters : Unbounded_String;
--        end record;

--
--     type Position_Pair is
--        record
--           First : Positive;
--           Last  : Natural;
--        end record;
--
--     function Token (Source : String; Interval : Position_Pair)
--                     return String
--     is
--     begin
--        return Source(Interval.First .. Interval.Last);
--     end Token;

--     function To_Inet_Addr (X : String)
--                            return Inet_Addr_Type
--     is
--        function Is_Numeric_Addr (X : String)
--                                  return Boolean
--        is
--           Num_Dots : Natural := 0;
--           Dot_Seen : Boolean;
--           Num_Digit : Natural := 0;
--        begin
--           if (X'Length = 0 or else not (X(1) in Decimal_Digit_Set)) then
--              return False;
--           end if;
--
--           Dot_Seen := False;
--           Num_Dots := 0;
--           Num_Digit := 0;
--           for I in X'Range loop
--              if (X (I) = '.') then
--                 if (Dot_Seen or Num_Dots = 3) then
--                    return False;
--                 end if;
--
--                 Dot_Seen := True;
--                 Num_dots := Num_Dots + 1;
--                 Num_Digit := 0;
--              elsif (not (X (I) in Decimal_Digit_Set)) then
--                 return False;
--              elsif (Num_Digit = 3) then
--                 return False;
--              else
--                 Dot_Seen := False;
--                 Num_Digit := Num_Digit + 1;
--              end if;
--           end loop;
--
--           return not Dot_Seen;
--        end Is_Numeric_Addr;
--     begin
--
--     exception
--        when Host_Error =>
--           raise;
--     end To_Inet_Addr;

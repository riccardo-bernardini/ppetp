--
-- Description: a mini HTTP client
-- Status : <UNTESTED>
--
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;
with HTTP_Syntax;
--
-- This package implements an "URL downloader", that is, an object
-- that can be used to download contents via HTTP.  Currently this
-- package is not very flexible (for example, it cannot use "POST",
-- but only "GET"), but it should be enough for many applications.
--
-- Typical usage:
--
--        Downloader : URL_Downloader;
--
--        Downloader.Get("http://www.yahoo.com/");  -- get the contents
--
--        put_line(Downloader.header("Content-Type")); -- Print an header
--        Put_Line(Downloader.Content);                -- print the body
--

package URL_Downloaders is
   type URL_Downloader is private;

   type Header_Array is array (Natural range <>) of Unbounded_String;

   --
   -- Download via method "GET" the content of URL.  Add the header
   -- lines in Headers to the request.  CRLF are automatically added
   -- after each header.
   --
   procedure Get
     (Downloader : in out URL_Downloader;
      URL        : String;
      Headers    : Header_Array);

   -- As the procedure above, but no additional header are added
   procedure Get
     (Downloader : in out URL_Downloader;
      URL        : String);

   -- Return the body of the most recently downloaded content.
   -- Raise No_Content if called before Get.
   function Content (Downloader : URL_Downloader)
                     return String;

   -- Return the value associated with header Name.  Raise No_Such_Header
   -- if there is no header with name Name.  The values of multiple headers
   -- with the same name are concatenated separated by "," (according to
   -- the HTTP specs).
   -- Raise No_Content if called before Get.
   function Header  (Downloader : URL_Downloader;
                     Name       : String)
                     return String;

   -- Return true if header Name exists.
   -- Raise No_Content if called before Get.
   function Exist_Header (Downloader : URL_Downloader;
                          Name       : String)
                          return Boolean;

   No_Such_Header       : exception;
   No_Content           : exception;
   Unimplemented_Scheme : exception;
private
   package Header_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => String);



   type URL_Downloader is
      record
         URL     : Unbounded_String := Null_Unbounded_String;
         Headers : Header_Maps.Map;
         Content : Unbounded_String;
         Status  : HTTP_Syntax.HTTP_Status;
      end record;

end URL_Downloaders;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
use  Ada.Strings;

with Ada.Strings.Maps;
use  Ada.Strings.Maps;

with Text_Io;
use  Text_Io;

package body Length_Delimited_Strings is
   --
   -- Extract the next LDS from Source, starting from From.
   -- Return the extracted string in Result and the index
   -- of the next character to be read in Next.  If no
   -- LDS is found, return 0 in Next and Result="";
   --
   procedure Extract_LDS (Source : in     String;
                          Result : in out Unbounded_String;
                          Next   :    out Natural;
                          From   : in     Natural) is
      Digit : Character_Set := To_Set(Span => (Low  => '0',
                                               High => '9'));
      Len_First : Natural;
      Len_Last  : Natural;
      Length    : Natural;

      Cursor    : Natural;
   begin
      -- Be pessimistic: suppose no LDS present
      Result := Null_Unbounded_String;
      Next   := 0;

      Cursor := From;



  Search_Length:
      loop
         if (Cursor > Source'Last) then
            -- Cursor points after the end of Source: nothing to do
            return;
         end if;

         -- Search for a digit
         Len_First := Index(Source => Source(Cursor..Source'Last),
                            Set    => Digit);

         if (Len_First = 0) then
            return;
         end if;

         -- Search for a non-digit
         Len_Last := Index(Source => Source(Len_First..Source'Last),
                           Set    => Digit,
                           Test   => Outside);


         if (Len_Last = 0) then
            return;
         end if;

         -- If the non digit is a colon, we found the string length
         exit when  (Source(Len_Last) = ':');

         -- otherwise... Try it again, Sam!
         Cursor := Len_Last+1;
      end loop Search_Length;

      Length := Natural'Value(Source(Len_First..Len_Last-1));

      if (Len_Last+Length > Source'Last) then
         raise Malformed_LDS;
      end if;

      Result := To_Unbounded_String(Source(Len_Last+1..Len_Last+Length));
      Next   := Len_Last+Length+1;
   end Extract_Lds;

   procedure Extract_LDS (Source : in     String;
                          Result : in out Unbounded_String;
                          Next   :    out Natural) is
   begin
      Extract_LDS (Source, Result, Next, Source'First);
   end Extract_LDS;


   procedure Load (LDS_Set : in out LDS_Set_Type;
                   Source  : in     String) is
      N_Colons : Natural;
   begin
      --
      -- We need to preallocate the buffer which will contain
      -- the extracted string.  We simply count the number of
      -- colons in the string since there is at most one
      -- colon per LDS.  Note that the actual number of LDS
      -- can be smaller than the number of colons as in
      --
      --   "3:foo 25:this string : has a colon ignored: 2:ab"
      --
      -- which has 5 colons but only 2 strings (the third colon
      -- is included in the second string, the fourth colon
      -- is ignored)
      --
      N_Colons := 0;
      for I in Source'Range loop
         if (Source(I) = ':') then
            N_Colons := N_Colons + 1;
         end if;
      end loop;

      LDS_Set := (Set  => new Unbounded_Str_Array(1..N_Colons),
                  Size => 0);

      if (N_Colons = 0) then
         -- If no colons are present, there is no LDS and we are done.
         return;
      end if;

      declare
         Next : Natural;
         From : Natural := Source'First;
      begin
         loop
            LDS_Set.Size := LDS_Set.Size + 1;

            Extract_LDS(Source => Source,
                        Result => LDS_Set.Set(LDS_Set.Size),
                        Next   => Next,
                        From   => From);

            From := Next;

            exit when (Next = 0) or (LDS_Set.Size = LDS_Set.Set'Last);
         end loop;

         if (Next = 0) then
            LDS_Set.Size := LDS_Set.Size - 1;
         end if;
      end;
   end Load;

   function N_Elem (LDS_Set : in LDS_Set_Type) return Natural is
   begin
      return LDS_Set.Size;
   end N_Elem;

   function Element (LDS_Set : in LDS_Set_Type;
                     Where   : in Positive) return String is
   begin
      return To_String(LDS_Set.Set(Where));
   end Element;

   function Encode(What : String) return String is
      Head : String := Integer'Image(What'Length);
   begin
      return Head(2..Head'Last) & ":" & What;
   end Encode;
end Length_Delimited_Strings;

with Ada.Containers.Indefinite_Ordered_Maps;

with Profiles.Entangled.Vandermonde;   use Profiles.Entangled.Vandermonde;
with Profiles.Entangled;   use Profiles.Entangled;
with PPETP;

--with Utility_Queue;   use Utility_Queue;
with Profiles_Utility;		use Profiles_Utility;
with Vandermonde_Utility;	use Vandermonde_Utility;


package Disentangler_Input_Queue is
  new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => IntSeqNum_StreamID_Key, -- combination of Sequence_number and Stream_ID
                                              Element_Type => Entangled_Record);

--  package String_Integer_Maps is
--  new Ada.Containers.Indefinite_Hashed_Maps
--  (String,
--  Integer,
--  Ada.Strings.Hash,
--  Equivalent_Keys => "=");




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
--
-- FROM: http://coding.derkeiler.com/Archive/Ada/comp.lang.ada/2006-11/msg00320.html
--
--      Can anybody explain me how to use associative arrays in Ada 2005? I
--      mean hashes like in Perl.
--
--
--  There are actually two kinds of "associative arrays" in Ada05: ordered maps and
--  hashed maps. Each of these container types accept both a key and element as
--  generic formal types. (There are also sets, that accept just an element type.)
--
--  If both the key and element are "definite" types (types that don't require a
--  constraint to specify the size), then you can use:
--
--  ada.containers.hashed_maps
--  ada.containers.ordered_maps
--
--  If either the key or element is an "indefinite" type (such as String), then you
--  can use:
--
--  ada.containers.indefinite_hashed_maps
--  ada.containers.indefinite_ordered_maps
--
--  If you use the hashed version, you'll need a hash function for your key type.
--  The library comes with a hash function for type String (and Wide_String, etc);
--  see Ada.Strings.Hash.
--
--  If you use the ordered version, you'll need a relation operation ("<") for your
--  key type.
--
--
--      In my opinion, C++ is cool but Ada's syntax is
--      better to avoid dirty code.
--
--
--  If you're familiar with the STL, then you should have no trouble with the Ada
--  container library.
--
--
--      So, when I say "hash", I mean something Perl-like like
--      "$hashname{key1}{key2} = 'something' ". Or something C++-like like "map
--      <string,int> hashname; hashname["key1"] = 42;".
--
--
--  Right. People often say "hash" but in reality that's just one way of
--  implementing an associative container. In Ada05 you can use either the hashed
--  version or the ordered version.
--
--  Your example above would be:
--
--  package String_Integer_Maps is
--  new Ada.Containers.Indefinite_Hashed_Maps
--  (String,
--  Integer,
--  Ada.Strings.Hash,
--  Equivalent_Keys => "=");
--
--  Note that there's no index operator (operator[]()) in Ada, so you say:
--
--  procedure Op (M : in out String_Integer_Maps.Map) is
--  begin
--  M.Include ("key1", 42);
--  end;
--
--  Note that this is not exactly equivalent to the C++ operation, since in Ada the
--  key is replaced too (if it already exists). That's unnecessary here so a
--  slightly more efficient technique might be:
--
--  procedure Op (M : in out String_Integer_Maps.Map) is
--  C : Cursor;
--  B : Boolean;
--  begin
--  M.Insert ("key1", 42, C, B);
--  if not B then
--  M.Replace_Element (C, 42);
--  end if;
--  end Op;
--
--
--      Is there anything similar in Ada 2005, too? I bought the book
--      "Programming in Ada 2005" by John Barnes but I don't understand the
--      author's way of explaining a programming language (for me, the book is
--      more confusing than explaining).
--
--
--  That book has a chapter (17?) on the container library. If you have a specific
--  question just post here or send me some email.
--
--
--      Or does anybody know a good tutorial similar to the lots of C- and
--      Perl-tutorials? I found many Ada-tutorials but most of them just list
--      the contents of the built-in packages and don't explain how to use
--      them. As a beginner this is confusing.
--
--
--  If just gave a tutorial on the Ada05 container library. I can give you the
--  power-point slides or send you a pdf version. Drop me a line if you're
--  interested.
--
--  But as a said above, the Ada standard container library is very similar to the
--  C++ STL, so if you already know the latter than the former shouldn't be much of
--  a stretch.
--
--  -Matt

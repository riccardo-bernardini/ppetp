with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Profiles.Config;        use Profiles.Config;

package body  Profiles.Generic_Parser is
   use Ada.Strings.Maps, Ada.Strings;

   function Downcase (X : String) return String is
      Lower : constant Character_Sequence :=
        To_Sequence(To_Set(Character_Range'(Low => 'a', High => 'z')));

      Upper : constant Character_Sequence :=
        To_Sequence(To_Set(Character_Range'(Low => 'A', High => 'Z')));

      Downcase_Map : constant Character_Mapping :=
        To_Mapping (From => Upper, To => Lower);
   begin
      return Fixed.Translate(X, Downcase_Map);
   end Downcase;


   procedure  Parse (Info_Table : in Parser_Info_Table;
                     Table      : in     Config_Table;
                     Result     :    out Parameters_Class_Pt;
                     Errors     : in out Config_Error_List) is


      Already_Found : array (Param_Name) of Boolean := (others => False);

      My_Result : Result_Pt := new Parameters_Type;
   begin
      -- Loop over all the (name, value) pairs
      for I in First(Table) .. Last(Table) loop
         declare
            Name    : String := Downcase (Get_Name(Table, I));
            Value   : String := Get_Value(Table, I);
            Ok      : Boolean := False;
            Reason  : Config_Err_Reason;
            Unknown : Boolean := True;
         begin
	    -- Search for a handler for Name
        Table_Search:
            for I in Param_Name loop
               if (Name = Info_Table(I).Name) then
                  Unknown := False;

                  if (Already_Found(I)) then
		     -- Name was found at least twice
                     Ok := False;
                     Reason := Multiple_Param;
                  else
		     -- Name found and it is the first time:
		     -- call the corresponding callback to
		     -- update My_Result value
                     Already_Found(I) := True;
                     Info_Table(I).Callback (Name   => Name,
                                             Value  => Value,
                                             Result => My_Result.all,
                                             Ok     => Ok,
                                             Reason => Reason);
                  end if;

		  exit Table_Search;
               end if;
            end loop Table_Search;

	    -- Handle error cases
            if (Unknown) then
               Errors.Append ((Name   => To_Unbounded_String(Name),
                               Value  => To_Unbounded_String(Value),
                               Reason => Param_Unknown));
	    elsif (not Ok) then
               Errors.Append ((Name   => To_Unbounded_String(Name),
                               Value  => To_Unbounded_String(Value),
                               Reason => Reason));
            end if;
         end;

	 -- Check if any mandatory parameter is missing
         for I in Param_Name loop
            if (not Already_Found(I) and Info_Table(I).Mandatory) then
               Errors.Append ((Name   => To_Unbounded_String(Param_Name'Image(I)),
                               Value  => To_Unbounded_String(""),
                               Reason => Param_Missing));
            end if;
         end loop;
      end loop;

      -- Finally, convert My_Result to the access class type
      Result := Parameters_Class_Pt(My_Result);
   end Parse;

end Profiles.Generic_Parser;

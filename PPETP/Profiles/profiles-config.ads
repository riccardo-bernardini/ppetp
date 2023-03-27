with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Profiles.Parameters;    use Profiles.Parameters;

package Profiles.Config is
   -- =================== --
   -- == Config Parser == --
   -- =================== --

   --
   -- The goal of a config parser is to map a set of
   -- (parameter-name, parameter-value) pairs to an element
   -- of Root_Parameters'Class type.  Such a goal is accomplished
   -- by a descendent of abstract type Root_Config_Parser.
   --

   type Root_Config_Parser is abstract new Root_Profile_Handler with null record;
   type Config_Class_Pt is access all Root_Config_Parser'Class;

   function New_Config_Parser (Profile : Profile_Type)
                              return Config_Class_Pt;

   --
   -- Root_Config_Parser defines an abstract procedure Parse
   -- which
   --
   --     + takes as input the list of (name, value) pairs and
   --     + returns
   --         (a) a Parameters_Class_Pt and
   --         (b) a (possibly empty) list of errors.  The possible
   --             error types are
   --
   --            * parameters whose name is unknown,
   --            * invalid parameter values
   --            * mandatory parameters missing
   --            * parameters given more than once
   --
   -- Before we can define Parse, we need to define the types for
   -- the input and output values of Parse.
   --

   --
   -- Config_Table will contain the (name, value) pairs.
   --
   type Config_Table is private;

   -- If an element with the same Name is founded it is overwritten
   procedure Insert (Table : in out Config_Table;
                     Name  : in     String;
                     Value : in     String);

   procedure Remove (Table : in out Config_Table;
                     Name  : in     String);

   function Size      (Table : Config_Table) return Natural;

   function First     (Table : Config_Table) return Natural;
   function Last      (Table : Config_Table) return Natural;

   function Get_Name  (Table : Config_Table;
                       Idx   : Positive) return String;

   function Get_Value (Table : Config_Table;
                       Idx   : Positive) return String;

   --
   -- The Error list is a vector of Config_Error
   --

   type Config_Err_Reason is (Param_Unknown,
                              Invalid_Value,
                              Param_Missing,
                              Multiple_Param);

   type Config_Error is
      record
         Name   : Unbounded_String;
         Value  : Unbounded_String;
         Reason : Config_Err_Reason;
      end record;

   package Conf_Err_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Config_Error);

   subtype Config_Error_List is Conf_Err_Vectors.Vector;
   subtype Config_Error_Idx  is Positive;

   -- type Junk is new config_table with null record;
   --
   -- Finally we can declare the Parse procedure
   --

   procedure Parse (Parser : in out Root_Config_Parser;
                    Table  : in     Config_Table;
                    Result :    out Parameters_Class_Pt;
                    Errors : in out Config_Error_List)

   is abstract;
private
   type Pair_Entry is
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   package Pair_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Pair_Entry);

   type Config_Table is
      record
         Data : Pair_Vectors.Vector;
      end record;

end Profiles.Config;

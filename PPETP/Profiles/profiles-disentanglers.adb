with Profiles.Disentanglers.Basic;
with Profiles.Disentanglers.Vandermonde;

with Disentangler_Input_Queue; use Disentangler_Input_Queue;
with Disentangler_Internal_Queue;   use Disentangler_Internal_Queue;
--with Utility_Queue;   use Utility_Queue;

package body Profiles.Disentanglers is
   ----------------------
   -- New_Disentangler --
   ----------------------

   function New_Disentangler(Profile : in Profile_Type;
                             Parameters : in Parameters_Class_Pt)
                             return    Disentangler_Class_Pt is
      Result : Disentangler_Class_Pt;
   begin
      case Profile is
         when Basic_Profile =>
            Result := Disentangler_Class_Pt (Basic.New_Disentangler);
         when Vandermonde_Profile =>
            Result := Disentangler_Class_Pt(Vandermonde.New_Disentangler);
      end case;


      Result.all.Parameters := Parameters;

      return Result;
   end New_Disentangler;

   ----------------------------
   -- New_Disentangler_Table --
   ----------------------------

--     function New_Disentangler_Table return Disentangler_Table is
--        Result : Disentangler_Table;
--     begin
--        for I in Profile_Type loop
--           Result (I) := New_Disentangler (I);
--        end loop;
--
--        return Result;
--     end New_Disentangler_Table;


   ---------
   -- Get --  NON TOCCARE!!!!!!!!!!!!!!!!!
   ---------

   procedure Get
     (Handler : in out Root_Disentangler;
      Result  : out Queue_Element)
   is
   begin
      if (Handler.Queue.Is_Empty) then
         raise Empty_Queue;
      end if;

      Result := Handler.Queue.First_Element;
      Handler.Queue.Delete_First;
   end Get;

   ---------------
   -- Any_Ready --
   ---------------

   function Any_Ready (Handler : in Root_Disentangler) return Boolean is
   begin
      return not Handler.Queue.Is_Empty;
   end Any_Ready;


-- >AS
--     ------------
--     -- Forget --
--     ------------
--
--     procedure Forget
--       (Handler   : in out Root_Disentangler'Class;
--        Requested : in PPETP.Timestamp_Type)
--     is
--     begin
--        Handler.Remove (Requested);
--        Handler.To_Be_Ignored.Insert
--          (Item         => Requested,
--           On_Duplicate => TS_Lists.Ignore);
--     end Forget;
-- < AS

   -------------
   -- Process --
   -------------

--     procedure Process
--       (Handler : in out Root_Disentangler'Class;
--        Data    : in Entangled_Data)
--     is
--        Ignored : Boolean;
--     begin
--        Handler.Process (Data => Data, Ready => Ignored);
--     end Process;


end Profiles.Disentanglers;

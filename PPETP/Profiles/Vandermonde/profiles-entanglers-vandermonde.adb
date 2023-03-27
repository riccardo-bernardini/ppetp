with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Builders;			use Profiles.Builders;



with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;

with ada.Text_IO;	use ada.Text_IO;


package body profiles.entanglers.vandermonde is
   -- =================== --
   -- ==== ENTANGLER ==== --
   -- =================== --


   -------------------
   -- New_Entangler --
   -------------------

   function New_Entangler return Entangler_Pt is
   begin
      return new Entangler;
   end New_Entangler;



   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Handler    : in out Entangler;
      Parameters : in     Parameters_Class_Pt)  is
   begin

      -- If the galois element is not valid generate a random one, but the
      -- Reduction Factor and the Field Size must be valid
      if(not Vandermonde_Par(Parameters.all).RF_Valid) or
        (not Vandermonde_Par(Parameters.all).GF_Valid) then

         raise No_Valid_Parameters;
      end if;


      if (not Vandermonde_Par(Parameters.all).GE_Valid) then
         Vandermonde_Par(Parameters.all).Galois_Element := Random_Galois;
         Vandermonde_Par(Parameters.all).GE_Valid := True;
      --   Put_Line("Entangler: Random: " & Image(Vandermonde_Par(Parameters.all).Galois_Element));
      end if;

      Profiles.Entanglers.Set_Default(Root_Entangler(Handler), Parameters);
      Profiles.Builders.Vandermonde.Set_Default(Handler.Internal_Builder.all, Parameters);
   end Set_Default;




   --------------
   -- Entangle --
   --------------
   procedure Entangle
     (Handler : in out Entangler;
      Input   : in     Application_Packet;
      Raw_Result  :    out Raw_Data) --Entangled_Data)
   is

      Application_Byte_Array_Pt : Byte_Array_Pt := new Byte_Array'(Input.Buffer);
      Padded_Data               : Byte_Array_Pt;
      Matrix_Of_Galois          : Matrix;
      Reduction_Vector          : Matrix;
      Result_Pt                 : Entangled_Payload_Pt;
      Padding_Flag              : Boolean := False;
      Base_Element              : Array_1D (1 .. 1);
      Inline_Out                : Boolean;
      Flags_Out                 : Profile_Flags;
      Result_Out                : Byte_Array_Pt;




      ----------------------------
      --- CONVERSION FUNCTIONS ---

      function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);


      function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);



      function Byte_Array_To_Stream_Array (Source : Entangler;
                                           Input : Byte_Array)
                                           return Ada.Streams.Stream_Element_Array is
      begin
         declare
            subtype Local_Array is Ada.Streams.Stream_Element_Array (1 .. Input'Length);
            Temp : Local_Array;
         begin
            for I in 1 .. Input'Length
            loop
               Temp(To_Stream_Offset(I)) := Input(To_Stream_Offset((I)));
            end loop;
            return Temp;
         end;
      end Byte_Array_To_Stream_Array;



      ------------------------
      --- FUNCTION PADDING ---

      function Padding (Handler : Entangler;
                        Input   : Byte_Array_Pt) return Byte_Array_Pt
      is
         Total_Len : Positive;
         R_Factor  : Natural := Vandermonde_Par(Entangler(Handler).
                                                  Default.all).Reduction_Factor;
         GF_S  : Natural := Vandermonde_Par(Entangler(Handler).
                                                  Default.all).GF_Size / 8;
         Padding_Length : Integer;

      begin

         if Input'Length mod (R_Factor * GF_S) = 0 then
            declare
               Padded_Array : Byte_Array_Pt := new Byte_Array'(Input.all);
            begin
               return Padded_Array;
            end;
         else
            Padding_Flag := True;
            Total_Len :=
                     R_Factor * GF_S * ((Input'Length / (R_Factor * GF_S)) + 1);
            declare
               Padded_Array : Byte_Array_Pt :=
                                 new Byte_Array (1 .. To_Stream_Offset(Total_Len));
            begin
               for I in 1 .. Total_Len
               loop
                  if I <= Input'Length then
                     Padded_Array(To_Stream_Offset(I)):= Input(To_Stream_Offset(I));
                  else
                     Padded_Array(To_Stream_Offset(I)):= 0;
                  end if;
               end loop;
               Padding_Length := Total_Len - Input'Length;
               if Padding_Length < 128 then
                  Padded_Array(To_Stream_Offset(Total_Len)) :=
                    To_Stream(Unsigned_8(Padding_Length));
               else
                  Padded_Array(To_Stream_Offset(Total_Len)) :=
                    To_Stream(Unsigned_8((Padding_Length mod 128) + 128));

                  Padded_Array(To_Stream_Offset(Total_Len - 1)) :=
                    To_Stream(Unsigned_8((Padding_Length - (Padding_Length mod 128))/ 128 ));

--                    Padded_Array(To_Stream_Offset(Total_Len)) :=
--                      To_Stream(Unsigned_8(0));
               end if;


               return Padded_Array;
            end;
         end if;
      end Padding;


      ------------------------
      --- FUNCTION MAPPING ---


      function Mapping (Handelr : Entangler;
                        Padded_Array  : Byte_Array_Pt) return Matrix is


         Data_Length : Integer := Padded_Array'Length;
         Galois_Exponent : Integer := Vandermonde_Par(Entangler(Handler).
                                                  Default.all).
                                                         GF_Size / 8;
         Padded_Galois_Array : Galois_Array
                              := Stream_To_Galois_Array
                               (Byte_Array_To_Stream_Array (Handler , Padded_Array.all));

         R : Natural := Vandermonde_Par(Entangler(Handler).
                                          Default.all).Reduction_Factor;

      begin
         declare
            Matrix_Of_Words : Matrix := Create(Padded_Galois_Array'Length , 1);
            Data_Matrix     : Matrix := Create(R , Padded_Galois_Array'Length / R);
         begin
            for Ic in 1 .. Padded_Galois_Array'Length
            loop
                  Set(Matrix_Of_Words, Ic , 1,
                      Padded_Galois_Array((Ic )));
            end loop;

            for Row in 1 .. R
            loop
               for Col in 1 .. Padded_Galois_Array'Length / R
               loop
                  Set(Data_Matrix, Row, Col, Get(Matrix_Of_Words, Row + R * (Col - 1)));
               end loop;
            end loop;

            return Data_Matrix;
         end;
      end Mapping;


   begin
    -- Padding --
      Padded_Data := Padding(Handler, Application_Byte_Array_Pt);
      Free(Application_Byte_Array_Pt);

    -- Mapping --
      Matrix_Of_Galois := Mapping(Handler , Padded_Data);

      Free(Padded_Data);
    -- Making reduction vector --
      Base_Element(1) := Vandermonde_Par(Entangler(Handler).Default.all).Galois_Element;

      Wandermonde(Reduction_Vector ,
                  Vandermonde_Par(Entangler(Handler).Default.all).Reduction_Factor ,
                  Base_Element);

    -- Making the Reduced_Vector and assigning results to Entangled_Data --
      Result_Pt :=
        Entangled_Payload_Pt'
          (new Vandermonde_Ent'(Galois_Data   => Reduction_Vector * Matrix_Of_Galois,
                                Padding => Padding_Flag,
                                Param => Vandermonde_Par(Entangler(Handler).Default.all)));


       --Result := (Timestamp => Input.Timetstamp,
         --         Payload   => Result_Pt);

      --Put_Line("Fin qua ci sono");

      Format_Data(Handler.Internal_Builder.all, Result_Pt, Inline_Out, Flags_Out, Result_Out);

      Free(Vandermonde_Ent_Pt(Result_Pt));
      --Raw_Result (Inline => Inline_Out,
      --                      Flags => Flags_Out,
      --                      Data => Result_Out);
      Raw_Result.Inline := Inline_Out;
      Raw_Result.Flags := Flags_Out;
      Raw_Result.Data := Result_Out;


   end Entangle;


end profiles.entanglers.vandermonde;

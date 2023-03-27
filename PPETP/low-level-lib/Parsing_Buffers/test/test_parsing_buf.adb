with Text_Io;            use Text_Io;
with Test_Report;        use Test_Report;
with Parsing_Buffers;    use Parsing_Buffers;
with Ada.Streams;        use Ada.Streams;

procedure Test_Parsing_Buf
is

   procedure Buffer_Size_Check (Buffer   : in out Parsing_Buffer;
                                Reporter : in out Reporter_Type;
                                Data     : in     Stream_Element_Array) is
      type Test_Case is
         record
            Len    : Stream_Element_Offset;
            Raised : Boolean;
         end record;

      At_Least_Cases : array (1 .. 3) of Test_Case :=
                         (1 => (Len => Data'Length,     Raised => False),
                          2 => (Len => Data'Length + 1, Raised => True),
                          3 => (Len => Data'Length - 1, Raised => False));

      Exactly_Cases  : array (1 .. 3) of Test_Case :=
                         (1 => (Len => Data'Length,     Raised => False),
                          2 => (Len => Data'Length + 1, Raised => True),
                          3 => (Len => Data'Length - 1, Raised => True));
      Raised : Boolean;
   begin
      Reporter.New_Suite ("Remaining");
      Reporter.New_Result (Buffer.Remaining = Data'Length);

      Reporter.New_Suite ("Empty");
      begin
         Raised := False;
         Buffer.Die_If_Not_Empty;
      exception
         when Length_Constraint =>
            Raised := True;
      end;

      Reporter.New_Result(Raised);

      Reporter.New_Suite ("At_Least");
      for I in At_Least_Cases'Range loop
         begin
            Raised := False;
            Buffer.Remain_At_Least_Or_Die (At_Least_Cases(I).Len);
         exception
            when Length_Constraint =>
               Raised := True;
         end;

         Reporter.New_Result (Raised = At_Least_Cases (I).Raised);
      end loop;

      Reporter.New_Suite ("Exactly");
      for I in Exactly_Cases'Range loop
         begin
            Raised := False;
            Buffer.Remain_Exactly_Or_Die (Exactly_Cases(I).Len);
         exception
            when Length_Constraint =>
               Raised := True;
         end;

         Reporter.New_Result (Raised = Exactly_Cases (I).Raised);
      end loop;

   end Buffer_Size_Check;

   procedure Data_Check (Buffer   : in out Parsing_Buffer;
                         Reporter : in out Reporter_Type;
                         Data     : in     Stream_Element_Array) is
      Ok : Boolean;
   begin
      Reporter.New_Suite ("Data");
      Ok := True;
      for I in Data'Range loop
         Ok := Ok and (Data (I) = Buffer.Data (I));
      end loop;

      Reporter.New_Result(Ok);
   end Data_Check;

   procedure Remove_Check (Buffer   : in out Parsing_Buffer;
                           Reporter : in out Reporter_Type;
                           Data     : in     Stream_Element_Array) is
      Raised    : Boolean;
      N_Removed : constant Stream_Element_Offset := 3;
   begin
      Reporter.New_Suite("Remove");
      Buffer.Remove_Last (3);

      begin
         Raised := False;
         Buffer.Remain_Exactly_Or_Die (Data'Length - N_Removed);
      exception
         when Length_Constraint =>
            Raised := True;
      end;

      Reporter.New_Result (not Raised);
      Reporter.New_Result (Buffer.Last = Data'Last - N_Removed);
   end Remove_Check;

   procedure Skip_Extract (Buffer   : in out Parsing_Buffer;
                           Reporter : in out Reporter_Type;
                           Data     : in     Stream_Element_Array) is
      procedure Get is
        new Extract (Stream_Element);

      Buf : Stream_Element;
      Remaining : Stream_Element_Offset;
   begin
      Remaining := Buffer.Remaining;
      Reporter.New_Suite("Skip & Extract");
      for I in Stream_Element_Offset range 1 .. 2 loop
         Get (Buffer, Buf);
         Reporter.New_Result (Buf = Data (I));
      end loop;

      Buffer.Skip (3);
      for I in Stream_Element_Offset range 1 .. 2 loop
         Get (Buffer, Buf);
         Reporter.New_Result (Buf = Data (I + 5));
      end loop;

      Reporter.New_Result (Buffer.Remaining = Remaining-7);
   end Skip_Extract;

   procedure Fill_Check (Buffer   : in out Parsing_Buffer;
                         Reporter : in out Reporter_Type;
                         Data     : in     Stream_Element_Array) is
      Buf1 : Stream_Element_Array (1 .. 3);
      Buf2 : Stream_Element_Array (1 .. 4);

      type Stream_Pt is access Stream_Element_Array;
      procedure Get is
        new Get_Remaining (Stream_Pt);

      Pt : Stream_Pt;
   begin
      Reporter.New_Suite ("Fill");
      Buffer.Fill (Buf1);
      Reporter.New_Result (Buf1 = Data (1 .. Buf1'Length));

      Buffer.Fill (Buf2);
      Reporter.New_Result (Buf2 = Data (Buf1'Length + 1 .. Buf1'Length + Buf2'Length));

      Reporter.New_Result (Buffer.Remaining = Data'Length-7);

      Reporter.New_Suite ("Get_Remaining");
      Get (Buffer, Pt);
      Reporter.New_Result (Pt /= null);
      Reporter.New_Result (Pt'Length = Data'Length - 7);
      Reporter.New_Result (Pt.all = Data(8..Data'Last));
   end Fill_Check;

   Data : Stream_Element_Array(1..10);
   Reporter : Reporter_Type;
begin
   -- Reporter.Be_Verbose;
   for I in Data'Range loop
      Data(I) := Stream_Element(I);
   end loop;

   declare
      Buffer : Parsing_Buffer := Make_Parsing_Buffer (Data);
   begin
      Reporter.New_Suite ("Last");
      Reporter.New_Result (Buffer.Last = Data'Last);

      Buffer_Size_Check (Buffer, Reporter, Data);
      Data_Check   (Buffer, Reporter, Data);
      Remove_Check (Buffer, Reporter, Data);
      Skip_Extract (Buffer, Reporter, Data);
   end;

   declare
      -- Make a fresh  buffer
      Buffer : Parsing_Buffer := Make_Parsing_Buffer (Data);
   begin
      Fill_Check (Buffer, Reporter, Data);
   end;

   Reporter.Final;
end Test_Parsing_Buf;

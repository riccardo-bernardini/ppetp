with Ada.Streams;	use Ada.Streams;
with Ada.Numerics.Discrete_Random;

with PPETP.Attributes;			use PPETP.Attributes;
with PPETP.Attributes.Puncturing;	use PPETP.Attributes.Puncturing;

with byte_arrays;			use byte_arrays;

-- only for print
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Characters.Latin_1;		use Ada.Characters.Latin_1;
with Network;					use Network;



package body PPETP.Targets is

   -- here we can use what we want, then the probability is
   -- calculated with a proportion
   MAX_RANDOM : constant Natural := 1000;

   type Random_Type is new Natural range 1 .. MAX_RANDOM;

   package Random_Routing is new Ada.Numerics.Discrete_Random (Random_Type);

   G : Random_Routing.Generator;


   use type Network.Sock_Addr_Type;

   ----------------
   -- New_Target --
   ----------------

   function New_Target (Address : Network.Sock_Addr_Type;
                        Output_Queue: Network_Packet_Queues.Queue_Pt;
                        Puncturing : Access_Attribute_Class) return Target is

      Result : Target := (Address => Address,
                          Output_Queue => Output_Queue,
                          Status  => Not_Ready,
                          Punct_Attr => Puncturing);
   begin

      return Result;
   end New_Target;





   -------------------
   -- Transmit_Data --
   -------------------
   function Transmit_Data (To   : Target;
                           Seq_Number: Data_Sequence_Number) return Boolean is

   begin

      -- Default behaviour
      if To.Punct_Attr = null then
         return True;
      end if;



      declare
         Data : Puncturing_Data(Get_Puncturing_Mode(Object => PUNCTURING_Attribute(To.Punct_Attr.all)));

      begin

         Get_Attribute(Object => PUNCTURING_Attribute(To.Punct_Attr.all),
                       Data   => Data);

         if Data.Mode = Probabilistic then  -- Probabilistic Mode

            declare
               Num, Den : byte_arrays.Byte;
            begin

               Num := Data.Num;
               Den := Data.Den;


                if Num = 0 then
                  return False;
               end if;

               if Natural(Num) > Natural(Den) then
                  return True;
               end if;

               -- Proportion for calculation of transmission probability
               if float(Random_Routing.Random(G)) < (float(MAX_RANDOM) * float(Num) / (float(Den)+1.0)) then
                  return True;
               else
                  return False;
               end if;
            end;

         else   -- Deterministic Mode

            declare
               Seq_Mod : Natural :=  Natural(Seq_Number) mod  (Natural(Data.M)+1);
            begin
               for i in Stream_Element_Offset range 1.. Stream_Element_Offset(Data.Size) loop
                  if Seq_Mod = Natural(Data.Val(i)) then
                     return True;
                  end if;
               end loop;

               return False;

            end;

         end if;
      end;


   end Transmit_Data;


   ----------
   -- Send --
   ----------
   procedure Send (To   : Target;
                   Item : Network_Packet;
                   Sequence_Num: Data_Sequence_Number) is
      Packet : Network_Packet := Item;
   begin
      if (To.Status /= Ready) then
         raise Target_Not_Ready;
      end if;

      if Transmit_Data(To         => To,
                       Seq_Number => Sequence_Num) then

         Set_Peer(Packet, To.Address);
         To.Output_Queue.Insert(Packet);
      end if;
   end Send;

   ---------------------
   -- Switch_To_Ready --
   ---------------------

   procedure Switch_To_Ready (T : in out Target) is
   begin
      case T.Status is
         when Ready =>
            raise Target_Ready;
         when Closed =>
            raise Target_Closed;
         when Not_Ready =>
            T.Status := Ready;
      end case;
   end Switch_To_Ready;

   -----------
   -- Close --
   -----------

   procedure Close (T : in out Target) is
   begin
      if (T.Status = Closed) then
         raise Target_Closed;
      end if;

      T.Status  := Closed;
   end Close;

   -------------
   -- Address --
   -------------

   function Address(T : in Target) return Network.Sock_Addr_Type is
   begin
      return T.Address;
   end Address;


   -----------
   -- Print --
   -----------
   procedure Print(T       : in Target;
                   Tab_Num : in Natural) is
      procedure Print_Tabs(Tab_Num : in Natural) is
      begin
         for i in Natural range 1 .. Tab_Num loop
            Put(Ada.Characters.Latin_1.HT);
         end loop;
      end Print_Tabs;
   begin

      declare
      begin
         Print_Tabs(Tab_Num);
         Put_Line("Address: " & Image(T.Address) );
         Print_Tabs(Tab_Num);
         Put_Line("Status : " & T.Status'img);
      exception
         when others =>
            Put_Line("exception in Target");
      end;

   end Print;

begin
   Random_Routing.Reset(G);


end PPETP.Targets;

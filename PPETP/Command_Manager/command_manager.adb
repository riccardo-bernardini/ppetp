with Ada.Text_IO;	use Ada.Text_IO;

package body Command_Manager is


   protected body Command_Manager_Handler is

      ---------------------
      -- Process_Command --
      ---------------------
      function Process_Command(PeerID  : in Peer_ID;
                               Seq_Num : in Command_Sequence_Number) return Command_Action is



         Index : Peer_ID_Cursor_Map.Cursor;
         Tmp_Lists: Peer_Packets_Info;
      begin

         Index := Peer_ID_Cursor_Map.Find(Container => List,
                                          Key       => PeerID);


         if  Index = Peer_ID_Cursor_Map.No_Element then
            -- If this PeerID does not exist we can set the packet in execution
            --         Put_Line("PC: No PeerID -> Execute");
            return Execute;
         else

            Tmp_Lists := Peer_ID_Cursor_Map.Element(Position => Index);

            -- Find if the packet is in execution
            if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Exe_List)) /= 0 then
               declare
                  Exe_List_Cursor : Packet_Info_List.Cursor;
                  Pkt : Packet_Info;
               begin

                  Exe_List_Cursor := Packet_Info_List.First(Container => Tmp_Lists.Exe_List);

                  Exe_Loop:
                  loop
                     Pkt := Packet_Info_List.Element(Exe_List_Cursor);

                     if Pkt.Seq_Num = Seq_Num then
                        --                     Put_Line("PC: Packet in Exe list -> Packet_In_Execution");
                        return Packet_In_Execution;
                     end if;

                     exit Exe_Loop when Exe_List_Cursor = Packet_Info_List.Last(Container => Tmp_Lists.Exe_List);

                     Exe_List_Cursor := Packet_Info_List.Next(Position => Exe_List_Cursor);
                  end loop Exe_Loop;

               end;
            end if;


            -- Find if the packet must be acked again or if acked too many time
            --   the packets in the Ack_List.
            if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Ack_List)) /= 0 then
               declare
                  First : Packet_Info;
                  Last  : Packet_Info;
                  Pkt   : Packet_Info;


                  Ack_List_Cursor : Packet_Info_List.Cursor;

                  Inside_Range: Boolean := False;
               begin

                  -- Before check all the list (some hole can exist) check if it
                  -- is in the interval of the list
                  First := Packet_Info_List.First_Element(Container => Tmp_Lists.Ack_List);
                  Last  := Packet_Info_List.Last_Element(Container => Tmp_Lists.Ack_List);

                  if First.Seq_Num > Last.Seq_Num then
                     if Seq_Num in First.Seq_Num .. Last.Seq_Num then
                        Inside_Range := True;
                     end if;
                  elsif First.Seq_Num < Last.Seq_Num then
                     if Seq_Num >= First.Seq_Num or Seq_Num <= Last.Seq_Num then
                        Inside_Range := True;
                     end if;
                  else  -- there is only one element
                     if First.Seq_Num = Seq_Num then
                        Inside_Range := True;
                     end if;
                  end if;

                  -- check all element of the list
                  if Inside_Range then

                     Ack_List_Cursor := Packet_Info_List.First(Container => Tmp_Lists.Ack_List);

                     Ack_Loop:
                     loop
                        Pkt := Packet_Info_List.Element(Ack_List_Cursor);

                        if Pkt.Seq_Num = Seq_Num then

                           if Pkt.Num_Ack < Max_ACK then
                              --                           Put_Line("PC: Packet in the Ack List -> Send Ack");
                              return Send_Ack;
                           else
                              --                           Put_Line("PC: Packet in the Ack List -> Packet Too Acked");
                              return Packet_Too_Acked;
                           end if;

                        end if;

                        exit Ack_Loop when Ack_List_Cursor = Packet_Info_List.Last(Container => Tmp_Lists.Ack_List);

                        Ack_List_Cursor := Packet_Info_List.Next(Position => Ack_List_Cursor);
                     end loop Ack_Loop;

                  end if;

               end;
            end if;


            -- Find if the packet is too old
            --  is the first packet Seq_Num in the Exe_List (if exists) or in the
            --  Ack table subtracted by the Oldest value
            declare

               First : Packet_Info;
               List_Cursor : Packet_Info_List.Cursor;
               Newest : Command_Sequence_Number;
            begin



               -- In one of the two list MUST exists a packet
               if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Exe_List)) /= 0 then
                  First := Packet_Info_List.First_Element(Container => Tmp_Lists.Exe_List);
                  Newest := First.Seq_Num;
               else
                  First := Packet_Info_List.First_Element(Container => Tmp_Lists.Ack_List);
                  Newest := First.Seq_Num;
               end if;

               if Natural(Newest) >= Natural(Oldest) then    -- 0 <= Oldest <= x  < Newest
                  if Seq_Num in Newest-Command_Sequence_Number(Oldest) .. Newest then

                     --                  Put_Line("PC: Packet not in lists -> Too Old");
                     return Packet_Too_Old;
                  else
                     --                  Put_Line("PC: Packet not in lists -> Execute");
                     return Execute; -- is a new packet but with a smaller Sequence_Number
                  end if;
               else
                  if Seq_Num >= Newest-Command_Sequence_Number(Oldest) or Seq_Num <= Newest then
                     --                  Put_Line("PC: Packet not in lists -> Too Old");
                     return Packet_Too_Old;
                  else
                     --                  Put_Line("PC: Packet not in lists -> Execute");
                     return Execute; -- is a new packet but with a smaller Sequence_Number
                  end if;
               end if;
            end;




         end if;

      end Process_Command;





      --------------
      -- Ack_Sent --
      --------------
      procedure Ack_Sent(PeerID  : in     Peer_ID;
                         Seq_Num : in     Command_Sequence_Number) is


         procedure Move_Exe_To_Ack(Tmp_Lists:        in out Peer_Packets_Info;
                                   Exe_List_Cursor : in out Packet_Info_List.Cursor) is
            Pkt_Exe : Packet_Info;
            Pkt_Ack : Packet_Info;
            Ack_List_Cursor : Packet_Info_List.Cursor;
         begin


            --         Put_Line("** Move_Exe_To_Ack **");
            Pkt_Exe := Packet_Info_List.Element(Exe_List_Cursor);

            Pkt_Exe.Num_Ack := 1;



            -- if the ack list is full delete the last element
            if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Ack_List)) = Re_Ack then
               --            Put_Line("META:  Ack List full, delete last element");
               Packet_Info_List.Delete_Last(Container => Tmp_Lists.Ack_List);
            end if;


            -- Is the first packet of the Ack list
            if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Ack_List)) = 0 then
               --            Put_Line("META:  Ack List empty, insert element");
               Packet_Info_List.Prepend(Container => Tmp_Lists.Ack_List,
                                        New_Item  => Pkt_Exe);
               --            Put_Line("META:  Element deleted from Exe list");
               Packet_Info_List.Delete(Container => Tmp_Lists.Exe_List,
                                       Position  => Exe_List_Cursor);
               return;
            end if;

            -- find where to put the new element
            declare
               Inserted : Boolean := False;

            begin

               Ack_List_Cursor := Packet_Info_List.First(Container => Tmp_Lists.Ack_List);


               Ack_Loop:
               loop
                  Pkt_Ack := Packet_Info_List.Element(Ack_List_Cursor);

                  if Pkt_Exe.Seq_Num < Pkt_Ack.Seq_Num then
                     --                  Put_Line("META:  Position found ! ,  Insert element");
                     Packet_Info_List.Insert(Container => Tmp_Lists.Ack_List,
                                             Before    => Ack_List_Cursor,
                                             New_Item  => Pkt_Exe);
                     Inserted := True;
                  end if;

                  exit Ack_Loop when Ack_List_Cursor = Packet_Info_List.Last(Container => Tmp_Lists.Ack_List);

                  Ack_List_Cursor := Packet_Info_List.Next(Position => Ack_List_Cursor);
               end loop Ack_Loop;

               -- Insert as last element
               if not Inserted then
                  --               Put_Line("META:  Position not found,  Append element");
                  Packet_Info_List.Append(Container => Tmp_Lists.Ack_List,
                                          New_Item  => Pkt_Exe);
               end if;

            end;
            -- Delete from the Exe list
            --         Put_Line("META:  Element deleted from Exe list");
            Packet_Info_List.Delete(Container => Tmp_Lists.Exe_List,
                                    Position  => Exe_List_Cursor);
         end Move_Exe_To_Ack;


         Index : Peer_ID_Cursor_Map.Cursor;
         Tmp_Lists: Peer_Packets_Info;

      begin


         -- if the packet is in execution move it to the ack map
         Index := Peer_ID_Cursor_Map.Find(Container => List,
                                          Key       => PeerID);

         if  Index = Peer_ID_Cursor_Map.No_Element then
            return;
         end if;

         Tmp_Lists := Peer_ID_Cursor_Map.Element(Position => Index);


         -- Find if the packet is in execution
         if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Exe_List)) /= 0 then
            declare
               Exe_List_Cursor : Packet_Info_List.Cursor;
               Pkt : Packet_Info;
            begin

               --            Put_Line("SA:  Exe list not empty");
               Exe_List_Cursor := Packet_Info_List.First(Container => Tmp_Lists.Exe_List);

               Exe_Loop:
               loop
                  Pkt := Packet_Info_List.Element(Exe_List_Cursor);

                  if Pkt.Seq_Num = Seq_Num then
                     -- Move the packet from execution to ack
                     --                  Put_Line("SA:  Element in Exe list Founded");
                     Move_Exe_To_Ack(Tmp_Lists, Exe_List_Cursor);


                     -- Update the Command_Manager
                     --                  Put_Line("META:  Replaced element in the Command Manager");
                     Peer_ID_Cursor_Map.Replace_Element(Container => List,
                                                        Position  => Index,
                                                        New_Item  => Tmp_Lists);
                     return;
                  end if;

                  exit Exe_Loop when Exe_List_Cursor = Packet_Info_List.Last(Container => Tmp_Lists.Exe_List);

                  Exe_List_Cursor := Packet_Info_List.Next(Position => Exe_List_Cursor);
               end loop Exe_Loop;
            end;
         end if;

         -- Find if the packet is in the ack list

         if Natural(Packet_Info_List.Length(Container => Tmp_Lists.Ack_List)) /= 0 then
            declare
               Ack_List_Cursor : Packet_Info_List.Cursor;
               Pkt : Packet_Info;
            begin
               --            Put_Line("SA:  Ack list not empty");
               Ack_List_Cursor := Packet_Info_List.First(Container => Tmp_Lists.Ack_List);

               Ack_Loop:
               loop
                  Pkt := Packet_Info_List.Element(Ack_List_Cursor);

                  if Pkt.Seq_Num = Seq_Num then
                     --                  Put_Line("SA:  Element in Ack list Founded");
                     --increment the Number of Ack
                     Pkt.Num_Ack := Pkt.Num_Ack + 1;

                     Packet_Info_List.Replace_Element(Container => Tmp_Lists.Ack_List,
                                                      Position  => Ack_List_Cursor,
                                                      New_Item  => Pkt);

                     -- Update the Command_Manager
                     Peer_ID_Cursor_Map.Replace_Element(Container => List,
                                                        Position  => Index,
                                                        New_Item  => Tmp_Lists);

                     return;
                  end if;

                  exit Ack_Loop when Ack_List_Cursor = Packet_Info_List.Last(Container => Tmp_Lists.Ack_List);

                  Ack_List_Cursor := Packet_Info_List.Next(Position => Ack_List_Cursor);
               end loop Ack_Loop;


            end;
         end if;



      end Ack_Sent;


      -----------------------------
      -- Set_Packet_In_Execution --
      -----------------------------
      procedure Set_Packet_In_Execution(PeerID  : in     Peer_ID;
                                        Seq_Num : in     Command_Sequence_Number) is

         Index : Peer_ID_Cursor_Map.Cursor;
         Tmp_Lists: Peer_Packets_Info;
         Tmp_Pkt_Info: Packet_Info;
      begin

         Index := Peer_ID_Cursor_Map.Find(Container => List,
                                          Key       => PeerID);

         Tmp_Pkt_Info.Seq_Num := Seq_Num;
         Tmp_Pkt_Info.Num_Ack := 0;



         if Index = Peer_ID_Cursor_Map.No_Element then
            --the PeerID doesn't exist, create the lists

            --         Put_Line("SPIE:  New PeerID");
            Packet_Info_List.Prepend(Container => Tmp_Lists.Exe_List,
                                     New_Item  => Tmp_Pkt_Info);

            Peer_ID_Cursor_Map.Insert(Container => List,
                                      Key       => PeerID,
                                      New_Item  => Tmp_Lists);
         else

            Tmp_Lists := Peer_ID_Cursor_Map.Element(Position => Index);

            --         Put_Line("SPIE:   New Packet");
            Packet_Info_List.Prepend(Container => Tmp_Lists.Exe_List,
                                     New_Item  => Tmp_Pkt_Info);

            Peer_ID_Cursor_Map.Replace(Container => List,
                                       Key       => PeerID,
                                       New_Item  => Tmp_Lists);
         end if;


      end Set_Packet_In_Execution;


      -----------
      -- Image --
      -----------
      procedure Image is
         PeerID_Cursor : Peer_ID_Cursor_Map.Cursor;
         Info_Lists : Peer_Packets_Info;
         PeerID : Peer_ID;
      begin

         if Natural(Peer_ID_Cursor_Map.Length(List)) /= 0 then

            PeerID_Cursor := Peer_ID_Cursor_Map.First(Container => List);

            Peer_Loop:
            loop
               Info_Lists := Peer_ID_Cursor_Map.Element(PeerID_Cursor);
               PeerID     := Peer_ID_Cursor_Map.Key(PeerID_Cursor);

               declare
                  Ack_List_Cursor : Packet_Info_List.Cursor;
                  Exe_List_Cursor : Packet_Info_List.Cursor;
                  Pkt : Packet_Info;

               begin
                  Put_Line("PeerID: " & PeerID'img);



                  Put_Line("Acked:");

                  if Natural(Packet_Info_List.Length(Info_Lists.Ack_List)) /= 0 then
                     Ack_List_Cursor := Packet_Info_List.First(Container => Info_Lists.Ack_List);

                     Ack_Loop:
                     loop
                        Pkt := Packet_Info_List.Element(Ack_List_Cursor);

                        Put(Pkt.Seq_Num'img & " [" & Pkt.Num_Ack'img & "] **");
                        exit Ack_Loop when Ack_List_Cursor = Packet_Info_List.Last(Container => Info_Lists.Ack_List);

                        Ack_List_Cursor := Packet_Info_List.Next(Position => Ack_List_Cursor);
                     end loop Ack_Loop;
                  else
                     Put_Line("Ack_List Empty");
                  end if;

                  New_Line;
                  Put_Line("In Execution:");
                  if Natural(Packet_Info_List.Length(Info_Lists.Exe_List)) /= 0 then

                     Exe_List_Cursor := Packet_Info_List.First(Container => Info_Lists.Exe_List);

                     Exe_Loop:
                     loop
                        Pkt := Packet_Info_List.Element(Exe_List_Cursor);
                        Put(Pkt.Seq_Num'img & "**");

                        exit Exe_Loop when Exe_List_Cursor = Packet_Info_List.Last(Container => Info_Lists.Exe_List);

                        Exe_List_Cursor := Packet_Info_List.Next(Position => Exe_List_Cursor);
                     end loop Exe_Loop;
                  else
                     Put_Line("Exe_List Empty");
                  end if;


               end;

               --- PeerID
               New_Line(2);

               exit Peer_Loop when PeerID_Cursor = Peer_ID_Cursor_Map.Last(Container => List);

               PeerID_Cursor := Peer_ID_Cursor_Map.Next(Position => PeerID_Cursor);
            end loop Peer_Loop;
         else
            Put_Line("Command Manager Empty");

         end if;

      end Image;

      ----------------
      -- Set_Re_Ack --
      ----------------
      procedure Set_Re_Ack(Value: in     Natural := 20) is
      begin
         Re_Ack := Value;
      end Set_Re_Ack;

      --------------------
      -- Set_Max_Oldest --
      --------------------
      procedure Set_Max_Oldest(Value: in     Natural := 50000) is
      begin
         Oldest := Value;
      end Set_Max_Oldest;

      -----------------
      -- Set_Max_ACK --
      -----------------
      procedure Set_Max_ACK(Value: in     Natural := 8) is
      begin
         Max_ACK := Value;
      end Set_Max_ACK;


   end Command_Manager_Handler;

end Command_Manager;

--
with Ada.Text_IO;
with Splitter_Lib.Network;
with Splitter_Lib.Stream_Table;
with Splitter_Lib.Controller;
with Splitter_Lib.RTP;
with Splitter_Lib.RTP_Streams;
with Splitter_Lib.Players;
with Ada.Calendar;
with Splitter_Lib.Command_Parser;

with GNAT.Sockets;

package body Splitter_Lib.Splitter is
   use Splitter_Lib.Command_Parser;
   procedure Start(Config : Config_Data) is

   ------------------------------
   -- Remove_Timed_Out_Sources --
   ------------------------------

      procedure Remove_Timed_Out_Sources is
         Now : Ada.Calendar.Time := Ada.Calendar.Clock;
         To_Be_Removed : RTP_Streams.Stream_Array :=
                           Stream_Table.Timed_Out_Sources (Now);

         Ignored       : Boolean;
         Src           : RTP_Streams.Stream;
      begin
         for I in To_Be_Removed'Range loop
            Src := To_Be_Removed(I);
	    Ignored := Controller.Kill_Player(RTP_Streams.Player_Id(Src));
            Stream_Table.Remove (Src);
            RTP_Streams.Close(Src);
	 end loop;
      end Remove_Timed_Out_Sources;

      ---------------
      -- To_Player --
      ---------------

      function To_Player (SSRC : RTP.SSRC_Type)
			 return RTP_Streams.Stream is
	 Src : RTP_Streams.Stream;
      begin
	 Src := Stream_Table.Find(SSRC);
         if RTP_Streams.Is_Void(Src) then
            Ada.Text_Io.Put_Line ("Nuovo SSRC:" & RTP.SSRC_Type'Image (Ssrc));
	    declare
	       Player_Port : Natural := Network.Free_UDP_Port;
	       Player      : Players.Player_Descriptor;
            begin
               Ada.Text_Io.Put_Line("--++--++--");
               Player := Controller.Start_Player(Player_Port);
               Ada.Text_Io.Put_Line("partito");
               Src := RTP_Streams.Create (SSRC, Player);
               Ada.Text_Io.Put_Line("creato");
               Stream_Table.Add(Src);
               Ada.Text_Io.Put_Line("inserito");
	    end;
	 end if;

	 return Src;
      end To_Player;

      Input  : Network.UDP_Socket_List := Network.UDP_List;
      Packet : Network.Packet_Buffer;
      SSRC   : RTP.SSRC_Type;
      Self_SSRC : RTP.SSRC_Type := RTP.No_SSRC;
      To_Target : Network.Output_UDP_Socket;

      Internal_Source : Natural := Network.No_Source;
      External_Source : Natural := Network.No_Source;
      Source          : Natural;
      use type RTP.SSRC_Type;
   begin
      --Ada.Text_IO.Put_Line(GNAT.Sockets.Image(Network.Self_IP_Address) & ":" & Config.Source_Port'img);
      --Network.Append (Input, Network.Self_IP_Address, Config.Source_Port, External_Source);
      Network.Append (Input, Config.Ext_Address, Config.Source_Port, External_Source);



      if Config.Target /= Command_Parser.No_Connection then
         Network.Append (Input, Network.any_addr, Config.Target.Internal_Port, Internal_Source);
         To_Target := Network.UDP_Output (Config.Target.Remote_Host.Addr,
                                          Integer(Config.Target.Remote_Host.Port));

         Ada.Text_IO.Put_Line("SPLITTER:  Mirror port: " & Config.Target.Internal_Port'img);
         Ada.Text_IO.Put_Line("SPLITTER:  external port: " & Config.source_Port'img);
         Ada.Text_IO.Put_Line("SPLITTER:  Remote addr: " & GNAT.Sockets.Image(Config.Target.Remote_Host.Addr) );
         Ada.Text_IO.Put_Line("SPLITTER:  Remote port: " & Config.Target.Remote_Host.Port'img);
      end if;

      Controller.Use_Port(Config.Control_Port);
      Stream_Table.Use_Timeout(Config.Timeout);  -- in seconds


      loop
         Network.Read_With_Timeout (From    => Input,
                                    To      => Packet,
                                    Source  => Source,
                                    Timeout => Stream_Table.Earliest_Timeout);

--         Ada.Text_IO.Put_Line ("GIGI");

         if (not Network.Is_Empty (Packet)) then
            -- Ada.Text_Io.Put_Line ("Letto: " & Integer'Image(Source));
            SSRC := RTP.Get_SSRC (Network.Data (Packet));
           -- Ada.Text_Io.Put_Line (RTP.SSRC_Type'Image (Ssrc) & " -- "
           --                      & RTP.SSRC_Type'Image (self_Ssrc) );
--              ada.Text_IO.Put_Line("SRC=" & natural'image(source)
--                                   & "e=" & natural'image(external_source)
--                                     & " i=" & natural'image(internal_source));
            if Source = External_Source then
               if SSRC /= Self_SSRC then
                 -- Ada.Text_Io.Put_Line (RTP.SSRC_Type'Image (Ssrc));
                  RTP_Streams.Send (Packet, To_Player (SSRC));
                  Stream_Table.Reset_Timeout (SSRC);
               end if;
            elsif Source = Internal_Source then
               --Ada.Text_IO.Put_Line("Miei");
               if ssrc /= self_ssrc then
                  Ada.Text_Io.Put_Line ("NEW: " & RTP.SSRC_Type'Image (Ssrc) & " -- "
                                        & RTP.SSRC_Type'Image (self_Ssrc) );
               end if;
               Self_SSRC := SSRC;
               Network.Send (What => Packet, Dst => To_Target);
            else
               -- We should never arrive here
               raise Program_Error;
            end if;
	 end if;

	 Remove_Timed_Out_Sources;
      end loop;
   end Start;
end Splitter_Lib.Splitter;

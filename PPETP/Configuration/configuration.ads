--                              -*- Mode: Ada -*-
--  Filename        : configuration.ads
--  Description     : Definitions used to represent a session config
--  Author          : Riccardo Bernardini
--  Created On      : Mon Nov 10 11:04:34 2008
--  Last Modified By: Roberto Cesco Fabbro
--  Last Modified On: Mon Feb 23 2009
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Network;               use Network;
with Auth.Profiles;         use Auth.Profiles, Auth;
with Auth.Credentials;      use Auth.Credentials;
with Profiles.Parameters;   use Profiles.Parameters;
with PPETP;		    use PPETP;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Configuration is

   --
   -- Part of the configuration is related to the credentials
   -- to be presented to a remote peer for starting/stopping
   -- data transmission.
   --

   type Command_Type is (Start, Stop, Any);

   type Command_Auth_Data is
      record
         Credentials : Auth_Data;     -- Give this credential...
         Command     : Command_Type;  -- ...to request this command
      end record;

   type Auth_Array is array (Positive range <>) of Command_Auth_Data;
   type Auth_Array_Pt is access Auth_Array;


   type Server_Config is
      record
         Addr : Inet_Addr_Type;
         Port : Port_Type;
         Id   : Integer;
      end record;

   type Server_Config_Pt is access Server_Config;


   --
   -- Information needed for the configuration of an input
   -- source.  We need the address (IP, port & channel) of
   -- the remote peer + authentication credentials
   --

   type Address_Type is (ip_type, ice_type);
   type ChannelID_Array is array (Positive range <>) of Channel_ID;
   type ChannelID_Array_Pt is access ChannelID_Array;





   --
   -- Informations necessary to configure a channel.  We need
   -- the processing profile associated to the channel and
   -- any parameters requested by the profile.
   --

   type Channel_Config is
      record
         Id         : Channel_ID;
         Profile    : Profiles.profile_type;
         Parameters : Parameters_Class_Pt;
         Auth       : Auth_Array_Pt;
      end record;


   type Output_Conf_Array is array (Positive range <>) of Channel_Config;
   type Channel_Config_Array_Pt is access Output_Conf_Array;


   type Input_Config is
      record
         Id        : Peer_ID;
         Addr_Type : Address_Type;
         Addr      : Inet_Addr_Type;
         Port      : Port_Type;
         Channels  : Channel_Config_Array_Pt;
         Auth      : Auth_Array_Pt;
      end record;

   type Input_Conf_Array is array (Positive range <>) of Input_Config;
   type Input_Config_Array_Pt is access Input_Conf_Array;


   function "=" (L, R : Input_Config) return Boolean;
   procedure Dump (X       : Input_Config;
                   Label   : String  := "";
                   Tabbing : Natural := 0);

   function "=" (L, R : Input_Conf_Array)return Boolean;


   function "=" (L, R : Channel_Config) return Boolean;
   procedure Dump (X     : Channel_Config;
                   Label : String := "";
                   Tabbing : Natural := 0);
   function "=" (L, R : Output_Conf_Array) return Boolean;

   --
   -- Informations necessary to configure a session.
   --

   type Session_Config is
      record
         Profile : Profiles.Profile_Type;
         Default_StreamID : Stream_ID;
         Parameters : Parameters_Class_Pt;
         Server  : Server_Config_Pt;
         Inputs  : Input_Config_Array_Pt; --(1 .. N_Inputs);
         Outputs : Channel_Config_Array_Pt;-- (1 .. N_Outputs);
      end record;

   type Session_Config_Pt is access Session_Config;

   function "=" (L, R : session_config) return Boolean;
   procedure Dump(X       : Session_Config;
                  Label   : String := "";
                  Tabbing : Natural := 0);

   type Config_Data is array(Positive range <>) of Session_Config_Pt;

   procedure Dump(X       : Config_Data;
                  Label   : String := "";
                  Tabbing : Natural := 0);

end Configuration;

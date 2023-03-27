with Ada.Containers.Indefinite_Ordered_Maps;

with Profiles.Entangled.Vandermonde;   use Profiles.Entangled.Vandermonde;
with Profiles.Entangled;   use Profiles.Entangled;
-- with Profiles.Disentanglers;   use Profiles.Disentanglers;
with PPETP;	use PPETP;

--with Utility_Queue;   use Utility_Queue;
with Profiles_Utility;	use Profiles_Utility;


package Disentangler_Internal_Queue is
  new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => SeqNum_StreamID_Key,
                                              Element_Type => Queue_Element);

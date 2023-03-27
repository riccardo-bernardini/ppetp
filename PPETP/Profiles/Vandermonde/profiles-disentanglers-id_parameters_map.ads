with Ada.Containers.Indefinite_Ordered_Maps;

with Profiles.Parameters.Vandermonde;	use Profiles.Parameters.Vandermonde;
with Vandermonde_Utility;		use Vandermonde_Utility;

package Profiles.Disentanglers.Id_Parameters_Map is
new Ada.Containers.Indefinite_Ordered_Maps(Key_Type     => Peer_Parameters_Key,
                                           Element_Type => Vandermonde_Par);

-- with Packets.Reduced.Binary;
-- use  Packets.Reduced.Binary;
--
-- with Packets.Components.Binary;
-- use  Packets.Components.Binary;
--
-- with Binary_Wandermonde_Oxidizers;
-- use  Binary_Wandermonde_Oxidizers;

with Generic_Oxidizers;

package Binary_Oxidizers is
   new Generic_Oxidizers (Reduced_Type       => Reduced_Binary,
                          Reduced_Type_Array => Reduced_Binary_Array,
                          Complete_Type      => Binary_Component,
                          Basic_Recoverer_Type => Wandermonde_Oxidizer_Type);


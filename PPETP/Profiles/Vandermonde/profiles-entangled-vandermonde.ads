with Byte_Arrays;     use Byte_Arrays;
with Galois_Matrices; use Galois_Matrices;

with Profiles.Parameters.Vandermonde;		use Profiles.Parameters.Vandermonde;

with Ada.Unchecked_Deallocation;

package  Profiles.Entangled.Vandermonde is

   type Vandermonde_Ent is
     new Root_Entangled_Payload with
      record
         Galois_Data : Matrix;
         Padding     : Boolean;  -- deriva dal flag presente nell'header del pacchetto.                                 --         Param       : Parameters_Class_Pt;
         Param       : Vandermonde_Par;
      end record;

   type Vandermonde_Ent_Pt is access all Vandermonde_Ent;

   function "=" (X, Y: Vandermonde_Ent) return Boolean;

   procedure Free is
     new Ada.Unchecked_Deallocation(Object => Vandermonde_Ent,
                                    Name   => Vandermonde_Ent_Pt);

end Profiles.Entangled.Vandermonde;

-- Vandermonde_ent deve essere un new Root_Entangled_Payload o
-- un new Entangled_Data? Perché in quest'ultimo caso vedrei anche
-- il Timestamp e il puntatore, mentre nel precedente li vedo?
--
-- NUOVA OSSERVAZIONE: ciò che ho scritto appena sopra si è verificato anche
-- nel momento di scegliere dove posizionare il puntatore a Paramaters.
-- I listati fin ora modificati fanno riferimento all'entangled sempre attraverso
-- un puntatore a Root_Entangled_Payload e di conseguenza l'entangled_Data non è
-- raggiungibile (almeno per quanto visto fin ora)! Quindi ho preferito mettere
-- il puntatore a parameters qui, dove è visibile. (e compila!!!)
--


-- COM'E' DEFINITO IL ROOT_ENTANGLED_PAYLOAD
--
--  package Profiles.Entangled is
--     -- =============== --
--     -- == Entangled == --
--     -- =============== --
--
--     type Root_Entangled_Payload is abstract new Root_Profile_Handler with null record;
--     type Entangled_Payload_Pt is access all Root_Entangled_Payload'Class;
--
--     type Entangled_Data is
--        record
--           Timestamp : PPETP.Timestamp_Type;
--           Payload   : Entangled_Payload_Pt;
--        end record;
--  end Profiles.Entangled;





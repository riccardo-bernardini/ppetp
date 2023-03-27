-- Funzione per la conversione da decimale ad esadecimale

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Characters;
use  Ada.Characters;

Hex : array(1..16) of Character;
Vet_Aus : Hex:=('0', '1', '2', '3', '4', '5', '6', '7', '8',
               '9', 'A', 'B', 'C', 'D', 'E', 'F');
procedure Conversione(Input : in Integer; Output : out Integer) is
begin
   Get(Input);
   Put(Input);
end Conversione;







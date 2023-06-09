package Blowfish_Test_Vectors is
  type Blowfish_Vector is
    record
       Key         : Blowfish_Block;
       Clear_Text  : Blowfish_Block;
       Cypher_Text : Blowfish_Block;
    end record;

   Test_Vectors : Blowfish_Vector :=
   (  1 => (Key         => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Clear_Text  => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Cypher_Text => (16#4E#, 16#F9#, 16#97#, 16#45#, 16#61#, 16#98#, 16#DD#, 16#78#)),
      2 => (Key         => (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#),
            Clear_Text  => (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#),
            Cypher_Text => (16#51#, 16#86#, 16#6F#, 16#D5#, 16#B8#, 16#5E#, 16#CB#, 16#8A#)),
      3 => (Key         => (16#30#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Clear_Text  => (16#10#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#),
            Cypher_Text => (16#7D#, 16#85#, 16#6F#, 16#9A#, 16#61#, 16#30#, 16#63#, 16#F2#)),
      4 => (Key         => (16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#),
            Clear_Text  => (16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#),
            Cypher_Text => (16#24#, 16#66#, 16#DD#, 16#87#, 16#8B#, 16#96#, 16#3C#, 16#9D#)),
      5 => (Key         => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Clear_Text  => (16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#),
            Cypher_Text => (16#61#, 16#F9#, 16#C3#, 16#80#, 16#22#, 16#81#, 16#B0#, 16#96#)),
      6 => (Key         => (16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#, 16#11#),
            Clear_Text  => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Cypher_Text => (16#7D#, 16#0C#, 16#C6#, 16#30#, 16#AF#, 16#DA#, 16#1E#, 16#C7#)),
      7 => (Key         => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Clear_Text  => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Cypher_Text => (16#4E#, 16#F9#, 16#97#, 16#45#, 16#61#, 16#98#, 16#DD#, 16#78#)),
      8 => (Key         => (16#FE#, 16#DC#, 16#BA#, 16#98#, 16#76#, 16#54#, 16#32#, 16#10#),
            Clear_Text  => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Cypher_Text => (16#0A#, 16#CE#, 16#AB#, 16#0F#, 16#C6#, 16#A0#, 16#A2#, 16#8D#)),
      9 => (Key         => (16#7C#, 16#A1#, 16#10#, 16#45#, 16#4A#, 16#1A#, 16#6E#, 16#57#),
            Clear_Text  => (16#01#, 16#A1#, 16#D6#, 16#D0#, 16#39#, 16#77#, 16#67#, 16#42#),
            Cypher_Text => (16#59#, 16#C6#, 16#82#, 16#45#, 16#EB#, 16#05#, 16#28#, 16#2B#)),
     10 => (Key         => (16#01#, 16#31#, 16#D9#, 16#61#, 16#9D#, 16#C1#, 16#37#, 16#6E#),
            Clear_Text  => (16#5C#, 16#D5#, 16#4C#, 16#A8#, 16#3D#, 16#EF#, 16#57#, 16#DA#),
            Cypher_Text => (16#B1#, 16#B8#, 16#CC#, 16#0B#, 16#25#, 16#0F#, 16#09#, 16#A0#)),
     11 => (Key         => (16#07#, 16#A1#, 16#13#, 16#3E#, 16#4A#, 16#0B#, 16#26#, 16#86#),
            Clear_Text  => (16#02#, 16#48#, 16#D4#, 16#38#, 16#06#, 16#F6#, 16#71#, 16#72#),
            Cypher_Text => (16#17#, 16#30#, 16#E5#, 16#77#, 16#8B#, 16#EA#, 16#1D#, 16#A4#)),
     12 => (Key         => (16#38#, 16#49#, 16#67#, 16#4C#, 16#26#, 16#02#, 16#31#, 16#9E#),
            Clear_Text  => (16#51#, 16#45#, 16#4B#, 16#58#, 16#2D#, 16#DF#, 16#44#, 16#0A#),
            Cypher_Text => (16#A2#, 16#5E#, 16#78#, 16#56#, 16#CF#, 16#26#, 16#51#, 16#EB#)),
     13 => (Key         => (16#04#, 16#B9#, 16#15#, 16#BA#, 16#43#, 16#FE#, 16#B5#, 16#B6#),
            Clear_Text  => (16#42#, 16#FD#, 16#44#, 16#30#, 16#59#, 16#57#, 16#7F#, 16#A2#),
            Cypher_Text => (16#35#, 16#38#, 16#82#, 16#B1#, 16#09#, 16#CE#, 16#8F#, 16#1A#)),
     14 => (Key         => (16#01#, 16#13#, 16#B9#, 16#70#, 16#FD#, 16#34#, 16#F2#, 16#CE#),
            Clear_Text  => (16#05#, 16#9B#, 16#5E#, 16#08#, 16#51#, 16#CF#, 16#14#, 16#3A#),
            Cypher_Text => (16#48#, 16#F4#, 16#D0#, 16#88#, 16#4C#, 16#37#, 16#99#, 16#18#)),
     15 => (Key         => (16#01#, 16#70#, 16#F1#, 16#75#, 16#46#, 16#8F#, 16#B5#, 16#E6#),
            Clear_Text  => (16#07#, 16#56#, 16#D8#, 16#E0#, 16#77#, 16#47#, 16#61#, 16#D2#),
            Cypher_Text => (16#43#, 16#21#, 16#93#, 16#B7#, 16#89#, 16#51#, 16#FC#, 16#98#)),
     16 => (Key         => (16#43#, 16#29#, 16#7F#, 16#AD#, 16#38#, 16#E3#, 16#73#, 16#FE#),
            Clear_Text  => (16#76#, 16#25#, 16#14#, 16#B8#, 16#29#, 16#BF#, 16#48#, 16#6A#),
            Cypher_Text => (16#13#, 16#F0#, 16#41#, 16#54#, 16#D6#, 16#9D#, 16#1A#, 16#E5#)),
     17 => (Key         => (16#07#, 16#A7#, 16#13#, 16#70#, 16#45#, 16#DA#, 16#2A#, 16#16#),
            Clear_Text  => (16#3B#, 16#DD#, 16#11#, 16#90#, 16#49#, 16#37#, 16#28#, 16#02#),
            Cypher_Text => (16#2E#, 16#ED#, 16#DA#, 16#93#, 16#FF#, 16#D3#, 16#9C#, 16#79#)),
     18 => (Key         => (16#04#, 16#68#, 16#91#, 16#04#, 16#C2#, 16#FD#, 16#3B#, 16#2F#),
            Clear_Text  => (16#26#, 16#95#, 16#5F#, 16#68#, 16#35#, 16#AF#, 16#60#, 16#9A#),
            Cypher_Text => (16#D8#, 16#87#, 16#E0#, 16#39#, 16#3C#, 16#2D#, 16#A6#, 16#E3#)),
     19 => (Key         => (16#37#, 16#D0#, 16#6B#, 16#B5#, 16#16#, 16#CB#, 16#75#, 16#46#),
            Clear_Text  => (16#16#, 16#4D#, 16#5E#, 16#40#, 16#4F#, 16#27#, 16#52#, 16#32#),
            Cypher_Text => (16#5F#, 16#99#, 16#D0#, 16#4F#, 16#5B#, 16#16#, 16#39#, 16#69#)),
     20 => (Key         => (16#1F#, 16#08#, 16#26#, 16#0D#, 16#1A#, 16#C2#, 16#46#, 16#5E#),
            Clear_Text  => (16#6B#, 16#05#, 16#6E#, 16#18#, 16#75#, 16#9F#, 16#5C#, 16#CA#),
            Cypher_Text => (16#4A#, 16#05#, 16#7A#, 16#3B#, 16#24#, 16#D3#, 16#97#, 16#7B#)),
     21 => (Key         => (16#58#, 16#40#, 16#23#, 16#64#, 16#1A#, 16#BA#, 16#61#, 16#76#),
            Clear_Text  => (16#00#, 16#4B#, 16#D6#, 16#EF#, 16#09#, 16#17#, 16#60#, 16#62#),
            Cypher_Text => (16#45#, 16#20#, 16#31#, 16#C1#, 16#E4#, 16#FA#, 16#DA#, 16#8E#)),
     22 => (Key         => (16#02#, 16#58#, 16#16#, 16#16#, 16#46#, 16#29#, 16#B0#, 16#07#),
            Clear_Text  => (16#48#, 16#0D#, 16#39#, 16#00#, 16#6E#, 16#E7#, 16#62#, 16#F2#),
            Cypher_Text => (16#75#, 16#55#, 16#AE#, 16#39#, 16#F5#, 16#9B#, 16#87#, 16#BD#)),
     23 => (Key         => (16#49#, 16#79#, 16#3E#, 16#BC#, 16#79#, 16#B3#, 16#25#, 16#8F#),
            Clear_Text  => (16#43#, 16#75#, 16#40#, 16#C8#, 16#69#, 16#8F#, 16#3C#, 16#FA#),
            Cypher_Text => (16#53#, 16#C5#, 16#5F#, 16#9C#, 16#B4#, 16#9F#, 16#C0#, 16#19#)),
     24 => (Key         => (16#4F#, 16#B0#, 16#5E#, 16#15#, 16#15#, 16#AB#, 16#73#, 16#A7#),
            Clear_Text  => (16#07#, 16#2D#, 16#43#, 16#A0#, 16#77#, 16#07#, 16#52#, 16#92#),
            Cypher_Text => (16#7A#, 16#8E#, 16#7B#, 16#FA#, 16#93#, 16#7E#, 16#89#, 16#A3#)),
     25 => (Key         => (16#49#, 16#E9#, 16#5D#, 16#6D#, 16#4C#, 16#A2#, 16#29#, 16#BF#),
            Clear_Text  => (16#02#, 16#FE#, 16#55#, 16#77#, 16#81#, 16#17#, 16#F1#, 16#2A#),
            Cypher_Text => (16#CF#, 16#9C#, 16#5D#, 16#7A#, 16#49#, 16#86#, 16#AD#, 16#B5#)),
     26 => (Key         => (16#01#, 16#83#, 16#10#, 16#DC#, 16#40#, 16#9B#, 16#26#, 16#D6#),
            Clear_Text  => (16#1D#, 16#9D#, 16#5C#, 16#50#, 16#18#, 16#F7#, 16#28#, 16#C2#),
            Cypher_Text => (16#D1#, 16#AB#, 16#B2#, 16#90#, 16#65#, 16#8B#, 16#C7#, 16#78#)),
     27 => (Key         => (16#1C#, 16#58#, 16#7F#, 16#1C#, 16#13#, 16#92#, 16#4F#, 16#EF#),
            Clear_Text  => (16#30#, 16#55#, 16#32#, 16#28#, 16#6D#, 16#6F#, 16#29#, 16#5A#),
            Cypher_Text => (16#55#, 16#CB#, 16#37#, 16#74#, 16#D1#, 16#3E#, 16#F2#, 16#01#)),
     28 => (Key         => (16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#),
            Clear_Text  => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Cypher_Text => (16#FA#, 16#34#, 16#EC#, 16#48#, 16#47#, 16#B2#, 16#68#, 16#B2#)),
     29 => (Key         => (16#1F#, 16#1F#, 16#1F#, 16#1F#, 16#0E#, 16#0E#, 16#0E#, 16#0E#),
            Clear_Text  => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Cypher_Text => (16#A7#, 16#90#, 16#79#, 16#51#, 16#08#, 16#EA#, 16#3C#, 16#AE#)),
     30 => (Key         => (16#E0#, 16#FE#, 16#E0#, 16#FE#, 16#F1#, 16#FE#, 16#F1#, 16#FE#),
            Clear_Text  => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Cypher_Text => (16#C3#, 16#9E#, 16#07#, 16#2D#, 16#9F#, 16#AC#, 16#63#, 16#1D#)),
     31 => (Key         => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Clear_Text  => (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#),
            Cypher_Text => (16#01#, 16#49#, 16#33#, 16#E0#, 16#CD#, 16#AF#, 16#F6#, 16#E4#)),
     32 => (Key         => (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#),
            Clear_Text  => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Cypher_Text => (16#F2#, 16#1E#, 16#9A#, 16#77#, 16#B7#, 16#1C#, 16#49#, 16#BC#)),
     33 => (Key         => (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#),
            Clear_Text  => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#),
            Cypher_Text => (16#24#, 16#59#, 16#46#, 16#88#, 16#57#, 16#54#, 16#36#, 16#9A#)),
     34 => (Key         => (16#FE#, 16#DC#, 16#BA#, 16#98#, 16#76#, 16#54#, 16#32#, 16#10#),
            Clear_Text  => (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#),
            Cypher_Text => (16#6B#, 16#5C#, 16#5A#, 16#9C#, 16#5D#, 16#9E#, 16#0A#, 16#5A#))     );
end Blowfish_Test_Vectors;

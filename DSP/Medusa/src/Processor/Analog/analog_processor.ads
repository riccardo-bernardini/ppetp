with Generic_Processor;

package Analog_Processor is
   new Generic_Processor (Reduced_Type       => Reduced_Analog_Component,
                          Complete_Type      => Analog_Component,
                          Reconstructor_Type => Analog_Reconstructor,
                          Reducer_Type       => Analog_Reducer);



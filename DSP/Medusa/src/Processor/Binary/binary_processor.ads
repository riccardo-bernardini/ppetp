with Generic_Processor;

package Binary_Processor is
   new Generic_Processor (Reduced_Type       => Reduced_Binary_Component,
                          Complete_Type      => Binary_Component,
                          Reconstructor_Type => Binary_Reconstructor,
                          Reducer_Type       => Binary_Reducer);



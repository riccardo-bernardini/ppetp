with Generic_Tables;
with PPETP.Sources;

package Source_Tables is
   new Generic_Tables (Element => PPETP.Sources.Source,
                       Cursor  => PPETP.Source_ID);


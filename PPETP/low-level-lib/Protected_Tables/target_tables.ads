with Generic_Tables;
with PPETP.Targets;

package Target_Tables is
   new Generic_Tables (Element => PPETP.Targets.Target,
                       Cursor  => PPETP.Target_ID);

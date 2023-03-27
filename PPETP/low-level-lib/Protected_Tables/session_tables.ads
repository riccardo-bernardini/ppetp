with Generic_Tables;
with PPETP.Sessions;

package Session_Tables is
   new Generic_Tables (Element => PPETP.Sessions.Session,
                       Cursor  => PPETP.Session_ID);


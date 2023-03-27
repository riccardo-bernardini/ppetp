with Generic_Tables;
with PPETP.Channels;

package Channel_Tables is
   new Generic_Tables (Element => PPETP.Channels.Channel,
                       Cursor  => PPETP.Channel_ID);


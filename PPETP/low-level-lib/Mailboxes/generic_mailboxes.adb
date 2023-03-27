package body Generic_Mailboxes is
   protected body Mailbox is
      entry Wait (Result : out Return_Type) when Ready is
      begin
	 Result := Return_Value;
	 Ready  := False;
      end Wait;
      
      entry Done (Result : in Return_Type) when not Ready is
      begin
	 Return_Value := Result;
	 Ready := True;
      end Done;
   end Mailbox;
   
   type Access_Mailbox is access Mailbox;
end Generic_Mailboxes;

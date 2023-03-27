--                  -*- Mode: Ada -*-
--  Filename        : Actual_Make_Id.ads
--  Description     : Actual Make Id
--
--  Package for the istantiation of the Generic_Make_Id.
--
--  I define it here and not in Rply_Auth in how much otherwise
--  such program twice the same ID would create me.



with Generic_Make_Id;

package Actual_Make_id is new Generic_Make_Id(2);

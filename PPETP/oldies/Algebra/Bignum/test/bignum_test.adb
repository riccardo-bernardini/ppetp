with Test_Report;         use Test_Report;
with Text_Io;             use Text_Io;
with Big_Numbers;         use Big_Numbers;

procedure bignum_test is
   X, Y, Z : Big_Int;
begin
   X := To_Big_Int("111111111111111", 10);
   Y := To_Big_Int("222222222222222", 10);
   Z := X + Y;
   Put_Line (To_String(Z, 10));
   Z := X - Y;
   Put_Line (To_String(Z, 10));
end bignum_test;

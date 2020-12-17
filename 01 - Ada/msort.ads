package Msort is
  LENGTH : constant Integer := 40;
  subtype Num is Integer range -300 .. 300;
  type Nums is array (1 .. LENGTH) of Num;
  procedure Sort (A : in out Nums);
end Msort;

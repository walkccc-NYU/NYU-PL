with Ada.Text_IO, Msort;
use Ada.Text_IO, Msort;

procedure Main is
  package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
  use Int_IO;

  MyNums : Nums;
  MySum : Integer := 0;

  task Reader is
    entry Start;
  end Reader;

  task Sum is
    entry Start;
  end Sum;

  task Printer is
    entry Print_Nums;
    entry Print_Sum;
  end Printer;

  task body Reader is
  begin
    accept Start do
      for I in 1 .. Msort.LENGTH loop
        get(MyNums(I));
      end loop;
    end Start;
  end Reader;

  task body Sum is
  begin
    accept Start;
    for I in 1 .. Msort.LENGTH loop
      MySum := MySum + MyNums(I);
    end loop;
    Printer.Print_Sum;
  end Sum;

  task body Printer is
  begin
    loop
      select
        accept Print_Nums do
          for I in 1 .. Msort.LENGTH loop
            Put(MyNums(I), 4);
          end loop;
          Put_Line("");
        end Print_Nums;
      or
        accept Print_Sum do
          New_Line;
          Put("Total sum of the array: ");
          Put(MySum, 8);
          Put_Line("");
        end Print_Sum;
      or
        terminate;
      end select;
    end loop;
  end Printer;

begin
  Reader.Start;
  -- the Sort procedure shouldn't start running until Reader is finished
  Sort(MyNums);
  Sum.Start;
  Printer.Print_Nums;
end Main;

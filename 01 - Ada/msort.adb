with Ada.Text_IO;
use Ada.Text_IO;

package body Msort is
  procedure Sort(A : in out Nums) is
    package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
    use Int_IO;

    procedure Merge(L, M, R : in Integer) is
      B : Nums;               -- hold values of A
      I : Integer := L;       -- L's pointer [L..M]
      J : Integer := M + 1;   -- R's pointer [M + 1..R]
      K : Integer := L;       -- A's pointer [L..R]

    begin
      -- copy value from A to B
      for X in L .. R loop
        B(X) := A(X);
      end loop;

      -- merge
      while I <= M and J <= R loop
        if B(I) < B(J) then
          A(K) := B(I);
          I := I + 1;
          K := K + 1;
        else
          A(K) := B(J);
          J := J + 1;
          K := K + 1;
        end if;
      end loop;

      -- put remaining left part to A
      while I <= M loop
        A(K) := B(I);
        K := K + 1;
        I := I + 1;
      end loop;

      -- put remaining right part to A
      while J <= R loop
        A(K) := B(J);
        K := K + 1;
        J := J + 1;
      end loop;

    end Merge;

    procedure Merge_Sort(L, R : in Integer) is
      M : Integer;

      procedure Sort_Concurrently(L, M, R : in Integer) is
        task Sort_Left;
        task Sort_Right;

        task body Sort_Left is
        begin
          Merge_Sort(L, M);
        end Sort_Left;

        task body Sort_Right is
        begin
          Merge_Sort(M + 1, R);
        end Sort_Right;

      begin
        null;
      end;

    begin
      if L >= R then
        return;
      end if;

      M := (L + R) / 2;
      Sort_Concurrently(L, M, R);
      Merge(L, M, R);

    end Merge_Sort;

  begin
    Merge_Sort(1, LENGTH);
  end Sort;

end Msort;

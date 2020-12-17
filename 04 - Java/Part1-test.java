import java.util.*;

class SortedList<T extends Comparable<T>> extends ArrayList<T> implements Comparable<SortedList<T>> {
  @Override
  public boolean add(T e) {
    if (this.isEmpty()) {
      return super.add(e);
    }

    for (int i = 0; i < this.size(); ++i) {
      if (this.get(i).compareTo(e) > 0) {
        this.add(i, e);
        return true;
      }
    }

    return super.add(e);
  }

  @Override
  public int compareTo(SortedList<T> that) {
    final int n = Math.min(this.size(), that.size());

    for (int i = 0; i < n; ++i) {
      if (this.get(i).compareTo(that.get(i)) < 0) {
        return -1;
      }
    }

    if (this.size() < that.size())
      return -1;
    if (this.size() == that.size())
      return 0;
    return 1;
  }
}

class A implements Comparable<A> {
  Integer x;

  A(Integer x) {
    this.x = x;
  }

  Integer getComparator() {
    return x;
  }

  @Override
  public int compareTo(A that) {
    return getComparator().compareTo(that.getComparator());
  }

  @Override
  public String toString() {
    return "A<" + x + ">";
  }
}

class B extends A {
  Integer y;

  B(Integer x, Integer y) {
    super(x);
    this.y = y;
  }

  @Override
  Integer getComparator() {
    return x + y;
  }

  @Override
  public String toString() {
    return "B<" + x + "," + y + ">";
  }
}

class myPart1 {
  static <T> void addToSortedList(SortedList<? super T> L, T z) {
    L.add(z);
  }

  static void test() {
    SortedList<A> c1 = new SortedList<A>();
    SortedList<A> c2 = new SortedList<A>();

    for (int i = 35; i >= 0; i -= 5) {
      addToSortedList(c1, new A(i));
      addToSortedList(c2, new B(i + 2, i + 3));
    }

    System.out.print("c1: ");
    System.out.println(c1);

    System.out.print("c2: ");
    System.out.println(c2);

    switch (c1.compareTo(c2)) {
      case -1:
        System.out.println("c1 < c2");
        break;
      case 0:
        System.out.println("c1 = c2");
        break;
      case 1:
        System.out.println("c1 > c2");
        break;
      default:
        System.out.println("Uh Oh");
        break;
    }
  }

  static void stringTest() {
    SortedList<String> L = new SortedList<String>();

    L.add("Taiwan");
    L.add("Good");
    L.add("Test");
    L.add("Apple");
    L.add("Zebrb");
    L.add("Zebra");
    L.add("NYU");
    L.add("NTU");

    System.out.println(L);
  }

  static void sameTest() {
    SortedList<A> c1 = new SortedList<A>();
    SortedList<A> c2 = new SortedList<A>();

    for (int i = 10; i >= 0; i -= 2) {
      addToSortedList(c1, new A(i));
      addToSortedList(c2, new B(i / 2, i / 2));
    }

    addToSortedList(c1, new A(174));
    addToSortedList(c2, new B(87, 87));

    System.out.print("c1: ");
    System.out.println(c1);

    System.out.print("c2: ");
    System.out.println(c2);

    switch (c1.compareTo(c2)) {
      case -1:
        System.out.println("c1 < c2");
        break;
      case 0:
        System.out.println("c1 = c2");
        break;
      case 1:
        System.out.println("c1 > c2");
        break;
      default:
        System.out.println("Uh Oh");
        break;
    }
  }

  public static void main(String[] args) {
    test();
    stringTest();
    sameTest();
  }
}
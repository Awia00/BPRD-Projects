void main(int n) {
  int x;
  int y;
  int *a;
  int *b;
  a = &x;
  b = &y;
  *a = 5;
  *b = 10;
  
  print a;
  print *a;
  
  println;
  
  print b;
  print *b;
  
  println;
  println;
  println;
  
  a = b;
  print a;
  print *a;
  
  println;
  
  print b;
  print *b;
  
  println;
}
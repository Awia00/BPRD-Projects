int a[-20 .. 2 .. -10];
void main() {
  int i;
  i = 0;
  print |a|;
  println;
  while (i < 10) {
      print a[i];
      i=i+1;
  }
}

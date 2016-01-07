void main() {
  int arr[3];
  arr[0] = 1;
  arr[1] = 10;
  arr[2] = 100;
  int i;
  i = 0;
  int sum;
  sum = 0;
  while (i < |arr|) {
    sum = sum + arr[i];
    i = i+1;
  }
  print sum;
}

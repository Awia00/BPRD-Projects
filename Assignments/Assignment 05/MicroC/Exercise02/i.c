void main(int n) {
  int arr[10];
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  arr[5] = 6;
  arr[6] = 7;
  arr[7] = 8;
  arr[8] = 9;
  arr[9] = 10;
  int x;
  int *sum;
  sum = &x;
  *sum = 0;
  arrsum(n, arr, sum);
}

void arrsum(int n, int arr[], int *sum)
{
	int i;
  i = 0;
  while(i < n)
  {
    *sum = *sum + arr[i];
    i = i+1;
  }
  print *sum;
  println;
}
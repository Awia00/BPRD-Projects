void main(int n) {
  int arr[100];
  squares(n, arr);
  int x;
  int *sum;
  sum = &x;
  *sum = 0;
  arrsum(n, arr, sum);
}

void squares(int n, int arr[])
{
	int i;
	for(i = 0; i < n; i = i+1)
	{
		arr[i] = i*i;
		print arr[i];
		println;
	}
}

void arrsum(int n, int arr[], int *sum)
{
	int i;
  for(i = 0; i < n; i = i + 1)
  {
    *sum = *sum + arr[i];
  }
  print *sum;
  println;
}
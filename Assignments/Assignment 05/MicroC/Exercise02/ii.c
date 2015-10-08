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
	int i; i = 0;
	while(i<n)
	{
		arr[i] = i*i;
		print arr[i];
		println;
		i = i+1;
	}
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
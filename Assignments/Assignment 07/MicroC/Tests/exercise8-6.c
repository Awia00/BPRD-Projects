// test switch
void main() {
	int i;
	i = 2;
	switch (i)
	{
		case 1: { print 10; }
		case 2: { print 20; } // this should print
		case 3: { print 30; }
	}
	print i;
}
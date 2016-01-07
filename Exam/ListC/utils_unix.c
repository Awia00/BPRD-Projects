#include <sys/time.h>
#include <sys/resource.h>

int getUserTimeMs() {
	struct rusage ru;
	getrusage(RUSAGE_SELF, &ru);
	struct timeval t = ru.ru_utime;
	return (t.tv_sec * 1000 + t.tv_usec / 1000);
}

int* readfile(char* filename) {
  int capacity = 1, size = 0;
  int *program = (int*)malloc(sizeof(int)*capacity); 
  FILE *inp = fopen(filename, "r");
  int instr;
  while (fscanf(inp, "%d", &instr) == 1) {
    if (size >= capacity) { 
      int* buffer = (int*)malloc(sizeof(int) * 2 * capacity);
      int i;
      for (i=0; i<capacity; i++)
        buffer[i] = program[i];
      free(program);
      program = buffer;
      capacity *= 2;
    }
    program[size++] = instr;
  }
  fclose(inp);
  return program;
}

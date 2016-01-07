#include <windows.h>

int getUserTimeMs() {
	HANDLE hProcess = GetCurrentProcess();
	FILETIME ftCreation, ftExit, ftKernel, ftUser;
	SYSTEMTIME stUser;
	GetProcessTimes(hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser);
	FileTimeToSystemTime(&ftUser, &stUser);
	int result = stUser.wSecond * 1000 + stUser.wMilliseconds;
	return(result);
}

int* readfile(char* filename) {
	int capacity = 1, size = 0;
	int *program = (int*)malloc(sizeof(int)*capacity);
	FILE *inp;
	fopen_s(&inp, filename, "r");
	int instr;
	while (fscanf_s(inp, "%d", &instr) == 1) {
		if (size >= capacity) {
			int* buffer = (int*)malloc(sizeof(int) * 2 * capacity);
			int i;
			for (i = 0; i < capacity; i++)
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

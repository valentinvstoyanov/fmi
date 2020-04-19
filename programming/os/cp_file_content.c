//cp f1 f2

#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
	if (argc != 3) {
		printf("2 arguments expected!\n");
		return 0;
	}

	int fd1 = open(argv[1], O_RDONLY);
	if (fd1 < 0) {
		fprintf(stderr, "Failed to open %s for reading.\n", argv[1]);
		return -1;
	}

	int fd2 = open(argv[2], O_CREAT|O_WRONLY);
	if (fd2 < 0) {
		close(fd1);
		fprintf(stderr, "Failed to open %s for writing.\n", argv[2]);
		return -1;
	}

	char ch;
	while (read(fd1, &ch, 1)) {
		write(fd2, &ch, 1);
	}

	close(fd1);
	close(fd2);

	return 0;
}

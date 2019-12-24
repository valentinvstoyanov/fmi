//head -n 10 f

#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
	if (argc != 2) {
		printf("1 argument expected!\n");
		return 0;
	}

	int fd = open(argv[1], O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "Failed to open %s for reading.\n", argv[1]);
		return -1;
	}

	int i = 0;
	char ch;
	while (i != 10 && read(fd, &ch, 1)) {
		if (ch == '\n') {
			++i;
		}
		
		write(1, &ch, 1);
	}

	close(fd);

	return 0;
}

//wc f

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>

int main(int argc, char* argv[]) {
	if (argc != 2) {
		printf("1 argument expected\n");
		return 0;
	}

	int fd = open(argv[1], O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "Failed to open %s for reading.\n", argv[1]);
		return -1;
	}

	int chars = 0;
	int words = 0;
	int lines = 0;
	char ch;

	while (read(fd, &ch, 1)) {
		++chars;

		if (ch == '\n') {
			++words;
			++lines;
		}

		if (ch == ' ') {
			++words;
		}
	}

	close(fd);
	printf("%s has %d number of chars, %d number of words and %d number of lines\n", argv[1], chars, words, lines);

	return 0;
}

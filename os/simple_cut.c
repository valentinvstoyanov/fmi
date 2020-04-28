//cut -c m-n f

#include <stdio.h>
#include <fcntl.h> 
#include <unistd.h> 
#include <stdlib.h>

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("1 argument expected.\n");
        return 0;
    }

    int m, n;
    scanf("%d", &m);
    scanf("%d", &n);

    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        printf("Failed to open %s for reading.", argv[1]);
        return  0;
    }

    int c = 1;
    char ch;

    while (read(fd, &ch, 1)) {
        if ((c >= m && c <= n) || ch == '\n') {
            write(1, &ch, 1);
        }

        ++c;

        if (ch == '\n') {
            c = 1;
        }
    }

    close(fd);

    return 0;
}

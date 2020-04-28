all: texttransformer

texttransformer: main.o line.o text.o filemanager.o formatter.o
	g++ main.o line.o text.o filemanager.o formatter.o -std=gnu++0x -o texttransformer

main.o: main.cpp
	g++ -c main.cpp -std=gnu++0x

line.o: line.cpp
	g++ -c line.cpp -std=gnu++0x

text.o: text.cpp
	g++ -c text.cpp -std=gnu++0x

filemanager.o: filemanager.cpp
	g++ -c filemanager.cpp -std=gnu++0x

formatter.o: formatter.cpp
	g++ -c formatter.cpp -std=gnu++0x

clean:
	rm *o texttransformer

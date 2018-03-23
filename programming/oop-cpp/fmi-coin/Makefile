all: fmicoin

fmicoin: main.o file.o wallet.o transaction.o order.o command.o executor.o
	g++ main.o file.o wallet.o transaction.o order.o command.o executor.o -o fmicoin

main.o: main.cpp
	g++ -c main.cpp

file.o: file.cpp
	g++ -c file.cpp

wallet.o: wallet.cpp
	g++ -c wallet.cpp

transaction.o: transaction.cpp
	g++ -c transaction.cpp

order.o: order.cpp
	g++ -c order.cpp

command.o: command.cpp
	g++ -c command.cpp

executor.o: executor.cpp
	g++ -c executor.cpp
	
clean:
	rm *o fmicoin
	rm *.dat

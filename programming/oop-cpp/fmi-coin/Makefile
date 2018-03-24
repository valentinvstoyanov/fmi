all: fmicoin

fmicoin: main.o util.o wallet.o transaction.o order.o command.o performer.o
	g++ main.o util.o wallet.o transaction.o order.o command.o performer.o -o fmicoin

main.o: main.cpp
	g++ -c main.cpp

util.o: util.cpp
	g++ -c util.cpp

wallet.o: wallet.cpp
	g++ -c wallet.cpp

transaction.o: transaction.cpp
	g++ -c transaction.cpp

order.o: order.cpp
	g++ -c order.cpp

command.o: command.cpp
	g++ -c command.cpp

performer.o: performer.cpp
	g++ -c performer.cpp

clean:
	rm *o fmicoin
	rm *.dat

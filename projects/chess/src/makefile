TARGET=task
CC=g++
CFLAGS=-Wall
OBJECTS=battle.o client.o game.o main.o menu.o mycurses.o mysocket.o myutils.o server.o

all: $(TARGET) install clean
clean:
	rm -f *.o *~ $(TARGET)
$(TARGET): $(OBJECTS)
	$(CC) -o $(TARGET) $(CFLAGS) $(OBJECTS) -lcurses -lrt
install:
	cp $(TARGET) $(TARGET)1

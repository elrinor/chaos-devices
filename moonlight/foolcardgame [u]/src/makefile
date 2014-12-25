TARGET=fool
CC=g++
CFLAGS=-Wall
OBJECTS=Client.o Game.o main.o Socket.o Util.o Server.o

all: $(TARGET) install clean
clean:
	rm -f *.o *~ _$(TARGET)
$(TARGET): $(OBJECTS)
	$(CC) -o _$(TARGET) $(CFLAGS) $(OBJECTS) -lcurses -lrt
install:
	cp _$(TARGET) $(TARGET)

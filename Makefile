UART_FILE=UART_test.c
UART_OBJ=./_build/default/rel/iot/bin/driver
CC=gcc
FLAGS=-w -I.

all: driver

driver: $(UART_FILE)
	$(CC) -lpthread -o $(UART_OBJ) $^ $(FLAGS)

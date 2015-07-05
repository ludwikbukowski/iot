UART_FILE=UART_test.c
UART_MOCK=UART_mock.c
UART_OBJ=./priv/driver
UART_OBJ_MOCK=./priv/driver_mock
CC=gcc
FLAGS=-w -I.

all: driver driver_mock

driver: $(UART_FILE)
	$(CC) -lpthread -o $(UART_OBJ) $^ $(FLAGS)

driver_mock: $(UART_MOCK)
	$(CC) -lpthread -o $(UART_OBJ_MOCK) $^ $(FLAGS)
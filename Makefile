DRIVER = driver
MOCK = driver_mock
UART_FILE=c_src/UART_receiver.c
UART_MOCK=c_src/UART_mock.c
CURDIR := $(shell pwd)
UART_OBJ=$(CURDIR)/priv/driver
UART_OBJ_MOCK=$(CURDIR)/priv/driver_mock


CC=gcc
FLAGS=-w -I.

all: priv driver driver_mock

priv:
	mkdir priv

driver: $(UART_FILE)
	$(CC) -lpthread -o $(UART_OBJ) $^ $(FLAGS)

driver_mock: $(UART_MOCK)
	$(CC) -lpthread -o $(UART_OBJ_MOCK) $^ $(FLAGS)

clean:
	@rm -f $(UART_OBJ) $(DRIVER)
	@rm -f $(UART_OBJ_MOCK) $(MOCK)

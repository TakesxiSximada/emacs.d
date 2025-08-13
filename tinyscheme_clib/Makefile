CC = gcc
TINYSCHEME_SRC = $(HOME)/ng/tinyscheme
LIB_DIR = $(TINYSCHEME_SRC)
INCLUDE_DIR = $(TINYSCHEME_SRC)

libprocess.dylib: libprocess.c
	$(CC) \
		-I $(INCLUDE_DIR) \
		-L $(LIB_DIR) \
		-rpath $(LIB_DIR) \
		-ltinyscheme \
		-dynamiclib \
		-fpic \
		-o libprocess.dylib \
		libprocess.c

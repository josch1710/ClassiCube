TARGET			:=	ClassiCube
BUILD_DIR 	:=	build/os2
SOURCE_DIR	:=	src
BEARSSL			:=	1

C_SOURCES		:= $(wildcard $(SOURCE_DIR)/*.c)

BEARSSL_LIB := $(BUILD_DIR)/libbearssl.a
BEARSSL_SOURCES := $(wildcard third_party/bearssl/*.c)
BEARSSL_OBJECTS := $(patsubst third_party/bearssl/%.c, $(BUILD_DIR)/bearssl/%.o, $(BEARSSL_SOURCES))

ifeq ($(BEARSSL),1)
	C_SOURCES			+= $(wildcard third_party/bearssl/*.c)
endif

C_OBJECTS		:= $(patsubst $(SOURCE_DIR)/%.c, $(BUILD_DIR)/%.o, $(C_SOURCES))
ifeq ($(BEARSSL), 1)
	OBJECTS			:= $(C_OBJECTS) $(BUILD_DIR)/$(TARGET).res $(BEARSSL_LIB)
else
	OBJECTS			:= $(C_OBJECTS) $(BUILD_DIR)/$(TARGET).res
endif
#---------------------------------------------------------------------------------
# options for code generation
#---------------------------------------------------------------------------------
CC      := gcc
CFLAGS	:= -pipe -Wno-attributes -fno-math-errno -O3 -g -mtune=pentium4 -msse2 -march=i686 -idirafter /@unixroot/usr/include/os2tk45 -DOS2
LDFLAGS	:= -Zhigh-mem -Zomf -Zargs-wild -Zargs-resp -Zlinker DISABLE -Zlinker 1121
LIBS	:= -lcx -lmmpm2 -lpthread

ifeq ($(BEARSSL),1)
	LIBS += $(BEARSSL_LIB)
endif

default: $(TARGET).exe

$(TARGET).exe: $(BUILD_DIR) $(OBJECTS) misc/os2/classicube.def
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBS) misc/os2/classicube.def

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR) $(BUILD_DIR)/bearssl

$(BUILD_DIR)/%.o : $(SOURCE_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/$(TARGET).res: misc/os2/$(TARGET).rc misc/os2/$(TARGET).ico
	wrc -r misc/os2/$(TARGET).rc -fo=$@

# BearSSL objects
$(BUILD_DIR)/bearssl/%.o : third_party/bearssl/%.c | $(BUILD_DIR)/bearssl
	$(CC) $(CFLAGS) -c $< -o $@

$(BEARSSL_LIB): $(BEARSSL_OBJECTS)
	echo $(BEARSSL_OBJECTS)
	echo $(BEARSSL_SOURCES)
	$(AR) rcs $@ $^

.PHONY: clean
clean:
	$(RM) -rf $(TARGET).exe $(BUILD_DIR)

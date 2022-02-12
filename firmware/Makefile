# SDCC compiler name and path.
SDCC=sdcc
# MCU part number.
MCU_NUMBER=STM8S003F3
# MCU clock frequency
FREQ=16000000UL

# STM8 flash utility name and path.
FLASH=stm8flash
# Name of the STM8 programming device.
PROGRAMMER=stlinkv2
# MCU ID for flash utility.
MCU=stm8s003?3

# Linker parameters.
LDFLAGS=--out-fmt-ihx
# Compiler parameters.
CFLAGS=-D $(MCU_NUMBER) -D F_CPU=$(FREQ) -D INLINE_DELAY

# Name of the object files.
OBJ=main.rel 24c-eeprom.rel
# Name of the output file.
TARGET=ir-remote-clone.ihx
# Name of the fuse configuration file.
FUSE=fuse.bin

# Common object files for STM8 MCU series.
STD_OBJECTS=util/stm8-util.rel util/stm8-i2c.rel

OBJECTS= $(STD_OBJECTS) $(OBJ)

MAP=$(TARGET:.ihx=.map)
LK=$(TARGET:.ihx=.lk)
CDB=$(TARGET:.ihx=.cdb)

SYMBOLS=$(notdir $(OBJECTS:.rel=.sym))
ASMS=$(notdir $(OBJECTS:.rel=.asm))
LISTS=$(notdir $(OBJECTS:.rel=.lst))
RSTS=$(notdir $(OBJECTS:.rel=.rst))

all: $(TARGET)

%.rel: %.c
	$(SDCC) -mstm8 -lstm8 $(CFLAGS) -c $<

$(TARGET): $(OBJECTS)
	$(SDCC) -mstm8 -lstm8 $(LDFLAGS) -o $@ $(notdir $(OBJECTS))

clean:
	rm -f $(notdir $(OBJECTS))
	rm -f $(MAP)
	rm -f $(LK)
	rm -f $(CDB)
	rm -f $(SYMBOLS)
	rm -f $(ASMS)
	rm -f $(LISTS)
	rm -f $(RSTS)
	rm -f $(TARGET)

flash: $(TARGET)
	$(FLASH) -c $(PROGRAMMER) -p $(MCU) -s flash -w $(TARGET)

fuse:
	echo "00 00 ff 00 ff 00 ff 00 ff 00 ff" | xxd -r -p > $(FUSE)

flash-fuse:
	$(FLASH) -c $(PROGRAMMER) -p $(MCU) -s opt -w $(FUSE)

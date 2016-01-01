include config.mk

build:
	@echo -n Preprocessing the ucode into tmp.ucode...
	@${CC} -E ${INCLUDES} -P -x c entry.ucode -o tmp.ucode
	@echo done.
	@echo -n Assembling the ucode into ucode.bin...
	@rlasm -target=iris1 -input=tmp.ucode -output=ucode.bin
	@echo done.

clean:
	@echo -n Cleaning up microcode temporaries...
	@rm -f tmp.ucode ucode.bin
	@echo done.


ARCH=aarch64
ABI=linux-gnu

%.bin: %.o runtime/rts.o
	$(ARCH)-$(ABI)-gcc  -static -o $@ $< runtime/rts.o -nostdlib

%.o: %.s
	$(ARCH)-$(ABI)-as -o $@ $<

lambda_build/%.s: lambda_src/%.lambda
	cabal v2-run lambdac -- -o $@ $<

.PHONY: %.run

%.run: lambda_build/%.bin 
	@echo "----------------"
	@echo " Program Output "
	@echo "----------------"
	@echo
	@qemu-$(ARCH) $<
	@echo
	@echo "----------------"

lambda_build/%.s: lambda_src/%.lambda 
	cabal v2-run lambdac -- -o $@ $<

%.o: %.s
	as -g -o $@ $<

lambda_build/%.bin: lambda_build/%.o runtime/rts_macos.o
	ld -macosx_version_min 13.0.0 -o $@ $< runtime/rts_macos.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64

%.run: lambda_build/%.bin 
	./$<

.PHONY: %.run
.PRECIOUS: %.o
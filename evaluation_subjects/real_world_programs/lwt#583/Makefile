.PHONY: run_confuzz run_node_fz run_stress build stress_build clean pin_confuzz pin_crowbar pin_plain_lwt print_env

run_confuzz: build
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	timeout 1h afl-fuzz -d -i ip/ -o op/ ./test.out @@ || true

run_node_fz : build
	timeout 1h ./test.out -i || true

run_stress : stress_build
	timeout 1h ./stress_test.out -i || true

build: print_env pin_confuzz
	@eval $$(cat ./config); \
	ocamlfind opt -thread -afl-instrument -linkpkg -package lwt.unix,lwt_react test.ml -o test.out

stress_build: print_env pin_crowbar pin_plain_lwt 
	@eval $$(cat ./config); \
	ocamlfind opt -thread -afl-instrument -linkpkg -package lwt.unix,lwt_react,crowbar stress_test.ml -o stress_test.out

clean:
	rm -rf op/ *.cm* *.o *.out

pin_confuzz:
	opam pin add lwt -y -k path ../../../confuzz/; \
	opam pin add lwt_react -y -k path ../../../dependency/lwt_583_build/

pin_crowbar:
	opam pin add crowbar -y -k path ../../../dependency/crowbar

pin_plain_lwt:
	opam pin add lwt -y -k path ../../../dependency/vanilla_lwt

print_env:
	echo $$(opam env) > ./config


all: latc_x86_64
#all: frontend
#	mv frontend latc_x86_64

frontend: cargo_clean
	cargo build --release --bin type_check
	cp target/release/type_check frontend

cargo_clean:
	cargo clean -p latte

latc_x86_64: cargo_clean runtime
	cargo build --release --bin compiler
	cp target/release/compiler latc_x86_64
	cp latc_x86_64 latc_x86

runtime: lib/runtime.c
	gcc -O2 -c lib/runtime.c -o lib/runtime.o

clean: cargo_clean
	rm -f latc_x86_64 latc_x86 lib/runtime.o

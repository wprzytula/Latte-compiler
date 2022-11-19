all: latc_x86_64

cargo_clean:
	cargo clean

latc_x86_64: cargo_clean
	cargo build --release --bin compiler
	cp target/release/compiler latc_x86_64

clean: cargo_clean
	rm -f latc_x86_64

all: latc_x86_64
#all: frontend
#	mv frontend latc_x86_64

frontend: cargo_clean
	cargo build --release --bin type_check
	cp target/release/type_check frontend

cargo_clean:
	cargo clean -p latte

latc_x86_64: cargo_clean
	cargo build --release --bin compiler
	cp target/release/compiler latc_x86_64

clean: cargo_clean
	rm -f latc_x86_64

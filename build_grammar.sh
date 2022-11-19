set -e
GRAMMAR=src/grammars/ext/Latte.g4
java -jar antlr/antlr4-4.8-2-SNAPSHOT-complete-beta-0.3.jar -Dlanguage=Rust ${GRAMMAR} -Xexact-output-dir -visitor -o src/frontend/parser

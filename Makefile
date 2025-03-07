PARAM := `llvm-config --cxxflags --ldflags --libs core`

.PHONY: lang
lang:
	clang++ -g3 -o $@ -Wall ${PARAM} lang.cpp

PARAM := `llvm-config --cxxflags --ldflags --libs core` -rdynamic -Wl,--export-dynamic

.PHONY: lang
lang:
	clang++ -g3 -o $@ -Wall ${PARAM} lang.cpp

CXXFLAGS := -O0 -g3 -Wall `llvm-config --cxxflags`
LD_FLAGS := `llvm-config --ldflags --libs core` -rdynamic -Wl,--export-dynamic

EXE_NAME = lang

OBJS := parser.o compiler.o main.o

%.o: %.cpp
	clang++ ${CXXFLAGS} -c $<

$(EXE_NAME): $(OBJS)
	clang++ ${LD_FLAGS} -o $@ $(OBJS)

.PHONY: clean
clean:
	rm -f $(OBJS)
	rm -f $(EXE_NAME)

# lang

This is a small language with compilation to machine code using LLVM.

## Building

Type
```
  make
```

## Features
  * integer basic math
  * definition and calling of functions
  * compilation in memory - JIT

## Example
Run `./lang` and type:
```
  def a(x) { 10 * x; }; a(a(10 + 20) - 1);
```

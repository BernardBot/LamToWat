# LamToWat
Compile lambda expressions to [Web Assembly Text Format](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format).

## Running an example compilation
```
cabal install
cabal exec lam2wat ./test/lam/arith.lam out.wat
wat2wasm out.wat
wasm-interp out.wasm --run-all-exports
```
This should print:
```
_start() => i32:9
```
You can check the example `arith.lam` file and see that it multiplies three times three, which
indeed is nine.

## Running tests
You can run tests with `cabal test`.

# sheaf

> **Work in progress.** The design is settled in outline; only the first
> stage of the compiler exists.

**S**tatic **H**eapless **E**xpressions **A**s **F**orms. Sheaf is a
statically typed Lisp with no heap and no garbage collector. It uses
[Coalton](https://github.com/coalton-lang/coalton) as its frontend and a
memory model after [Goose](https://github.com/aardappel/goose): values live
on stacks the compiler assigns, and memory is freed when its scope ends.

## Documentation

| Document | Covers |
|---|---|
| [MEMORY.md](docs/MEMORY.md) | Data stacks, roots, and where values are built |
| [CLOSURES.md](docs/CLOSURES.md) | First-class functions without a heap |
| [PIPELINE.md](docs/PIPELINE.md) | The compiler's stages, and IR capture |
| [COALTON.md](docs/COALTON.md) | The Coalton fork and how it is updated |
| [STATUS.md](docs/STATUS.md) | What runs today |
| [TESTING.md](docs/TESTING.md) | Running the tests |

## Building

Needs SBCL and Quicklisp.

```sh
git clone --recursive git@github.com:takeiteasy/sheaf.git
cd sheaf
make test
```

## License

```text
The MIT License (MIT)

Copyright (c) 2026 George Watson

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

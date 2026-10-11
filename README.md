# sheaf

> **Work in progress.** Programs compile from Coalton to C today, but only
> for numbers and the data built from them. [STATUS.md](docs/STATUS.md) lists
> what runs.

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
| [IR.md](docs/IR.md) | The first-order IR the analyses read |
| [ROOTS.md](docs/ROOTS.md) | Checking that references outlive their stores |
| [NUMBERS.md](docs/NUMBERS.md) | Numeric types and what overflow does |
| [PRELUDE.md](docs/PRELUDE.md) | Arrays, slices and references |
| [STACKS.md](docs/STACKS.md) | Assigning data stacks to cells and growable values |
| [EMITTER.md](docs/EMITTER.md) | Emitting C and the data stack runtime |
| [MONOMORPHIZATION.md](docs/MONOMORPHIZATION.md) | Specializing at ground types and removing dictionaries |
| [COALTON.md](docs/COALTON.md) | The Coalton fork and how it is updated |
| [STATUS.md](docs/STATUS.md) | What runs today |
| [TESTING.md](docs/TESTING.md) | Running the tests |

## Building

Needs SBCL, Quicklisp and a C compiler.

```sh
git clone --recursive git@github.com:takeiteasy/sheaf.git
cd sheaf
make test
```

[TESTING.md](docs/TESTING.md) covers the tests and CI.

## License

```text
sheaf

Copyright (C) 2026 George Watson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
```

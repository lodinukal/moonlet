# moonlet

Parser, compiler and virtual machine for a typed lua dialect, inspired by luau.

## goals

- similar syntax to luau
- nominal typing, i.e. types are identified by their name and position in source code
- full type safety, code should not compile if it has type errors
- Embeddable in native applications
- Compiles to as many targets as possible
  - Planning for luau source code emit, hlsl, spirv, and llvm bitcode as targets

## Acknowledgements

- Based off the work by Ryan Liptak ([zua](https://github.com/squeek502/zua.git), BSD0)

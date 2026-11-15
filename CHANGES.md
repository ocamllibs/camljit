# 0.1

- Initial release: the JIT of ocip, as a library of its own.
- Windows amd64 (mingw-w64): the JIT is available (memory mapped with
  `VirtualAlloc`, symbols looked up by flexdll and among the exports of the
  DLLs, `movabsq` of symbol addresses in the encoder).

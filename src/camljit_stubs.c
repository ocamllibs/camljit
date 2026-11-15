/* Primitives of the JIT (see loader.ml): memory for the code and the data of
   the units, symbols of the process, and registration of the units in the
   runtime, as done by the native dynamic linker (runtime/dynlink_nat.c). */

#define CAML_INTERNALS
#define _GNU_SOURCE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/codefrag.h>
#include <caml/frame_descriptors.h>

/* Declared by caml/globroots.h for native code only. */
extern void caml_register_dyn_globals(void **globals, int nglobals);

#include <string.h>

#ifndef _WIN32
#include <dlfcn.h>
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

/* Addresses are represented by OCaml integers (user-space addresses fit in
   62 bits). */
#define Addr_val(v) ((char *) Long_val(v))
#define Val_addr(p) Val_long((intnat) (p))

#ifndef _WIN32

CAMLprim value camljit_page_size(value unit)
{
  return Val_long(sysconf(_SC_PAGESIZE));
}

/* Maps [size] bytes, readable and writable, preferably near [hint] (so that
   32-bit displacements can reach the process). */
CAMLprim value camljit_map(value hint, value size)
{
  void *p = mmap(Addr_val(hint), Long_val(size), PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANON, -1, 0);
  if (p == MAP_FAILED)
    caml_failwith(strerror(errno));
  return Val_addr(p);
}

CAMLprim value camljit_write(value address, value contents)
{
  memcpy(Addr_val(address), String_val(contents),
         caml_string_length(contents));
  return Val_unit;
}

/* Makes the memory executable (and read-only), or writable. The
   instruction cache is synchronized with the code written (needed on arm64,
   no-op on amd64). */
CAMLprim value camljit_protect(value address, value size, value executable)
{
  int prot = PROT_READ | (Bool_val(executable) ? PROT_EXEC : PROT_WRITE);
  if (mprotect(Addr_val(address), Long_val(size), prot) != 0)
    caml_failwith(strerror(errno));
  if (Bool_val(executable))
    __builtin___clear_cache(Addr_val(address),
                            Addr_val(address) + Long_val(size));
  return Val_unit;
}

/* The address of a symbol of the process, or 0. */
CAMLprim value camljit_dlsym(value name)
{
  return Val_addr(dlsym(RTLD_DEFAULT, String_val(name)));
}

/* The address of a function or of a data symbol of the program (for
   instance, of camljit itself), to place the units near it. */
CAMLprim value camljit_self_address(value unit)
{
  return Val_addr(&camljit_self_address);
}

#else

/* On Windows, the code is not loaded in memory ([Compile.available] is
   false: the code produced by the backend for Windows is not supported by
   the internal assembler and the loader): the primitives that depend on
   the system fail, so that camljit builds and its users fall back to
   external commands. */

static void camljit_unsupported(void)
{
  caml_failwith("camljit: loading code in memory is not supported on Windows");
}

CAMLprim value camljit_page_size(value unit)
{
  camljit_unsupported();
  return Val_unit;
}

CAMLprim value camljit_map(value hint, value size)
{
  camljit_unsupported();
  return Val_unit;
}

CAMLprim value camljit_write(value address, value contents)
{
  camljit_unsupported();
  return Val_unit;
}

CAMLprim value camljit_protect(value address, value size, value executable)
{
  camljit_unsupported();
  return Val_unit;
}

CAMLprim value camljit_dlsym(value name)
{
  return Val_addr(NULL);
}

CAMLprim value camljit_self_address(value unit)
{
  return Val_addr(&camljit_self_address);
}

#endif

/* Registers a unit in the runtime: its frame table (for the GC and the
   exceptions), its global roots, and its code (for marshaling closures and
   for backtraces). The arguments are addresses, 0 if absent. */
CAMLprim value camljit_register(value frametable, value gc_roots,
                                 value code_begin, value code_end)
{
  void *sym;
  if (Long_val(frametable) != 0)
    caml_register_frametable(Addr_val(frametable));
  if (Long_val(gc_roots) != 0) {
    sym = Addr_val(gc_roots);
    caml_register_dyn_globals(&sym, 1);
  }
  if (Long_val(code_begin) != 0 && Long_val(code_end) != 0
      && Long_val(code_begin) != Long_val(code_end))
    caml_register_code_fragment(Addr_val(code_begin), Addr_val(code_end),
                                DIGEST_LATER, NULL);
  return Val_unit;
}

/* Runs the initialization code of a unit, as caml_natdynlink_run. */
CAMLprim value camljit_run(value entry)
{
  CAMLparam1(entry);
  CAMLlocal1(result);
  void (*entrypoint)(void) = (void (*)(void)) Addr_val(entry);
  result = caml_callback((value) (&entrypoint), 0);
  CAMLreturn(result);
}

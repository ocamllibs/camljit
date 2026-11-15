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

#ifdef _WIN32
#include <stdint.h>
#include <stdio.h>
#include <windows.h>
#include <psapi.h>
#include <caml/osdeps.h>
#else
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

/* On Windows: the memory of the units is reserved and committed with
   VirtualAlloc, and the symbols of the process are looked up by flexdll
   (the symbols of the program, which flexlink exports, and of the units
   loaded by Dynlink), then in the modules (DLLs) loaded in the process
   (for instance, the functions of the C library called directly by the
   code, such as [sin]). */

static void camljit_fail_with_last_error(const char *what)
{
  char message[256];
  snprintf(message, sizeof(message), "camljit: %s failed (error %lu)", what,
           (unsigned long) GetLastError());
  caml_failwith(message);
}

CAMLprim value camljit_page_size(value unit)
{
  SYSTEM_INFO info;
  GetSystemInfo(&info);
  return Val_long(info.dwPageSize);
}

/* Maps [size] bytes, readable and writable, preferably near [hint] (so
   that 32-bit displacements can reach the process). Unlike mmap,
   VirtualAlloc does not take the address as a hint: the free regions from
   [hint] on are looked for with VirtualQuery (the addresses of the
   allocations are aligned on their granularity, 64 KB). Otherwise, the
   system chooses the address (the loader goes through stubs and the GOT
   when the process is out of reach). */
CAMLprim value camljit_map(value hint, value size)
{
  SYSTEM_INFO info;
  MEMORY_BASIC_INFORMATION region;
  uintptr_t granularity, length, address, limit;
  void *p = NULL;
  GetSystemInfo(&info);
  granularity = info.dwAllocationGranularity;
  length = Long_val(size);
  address = (Long_val(hint) + granularity - 1) & ~(granularity - 1);
  /* At most 1 GB after the hint: further, the units would be out of reach
     of the process anyway. */
  limit = address + ((uintptr_t) 1 << 30);
  while (p == NULL && address != 0 && address < limit
         && address < (uintptr_t) info.lpMaximumApplicationAddress
         && VirtualQuery((void *) address, &region, sizeof(region)) != 0) {
    uintptr_t region_end =
      (uintptr_t) region.BaseAddress + region.RegionSize;
    if (region.State == MEM_FREE && region_end - address >= length)
      p = VirtualAlloc((void *) address, length, MEM_RESERVE | MEM_COMMIT,
                       PAGE_READWRITE);
    if (p == NULL)
      address = (region_end + granularity - 1) & ~(granularity - 1);
  }
  if (p == NULL)
    p = VirtualAlloc(NULL, length, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (p == NULL)
    camljit_fail_with_last_error("VirtualAlloc");
  return Val_addr(p);
}

CAMLprim value camljit_write(value address, value contents)
{
  memcpy(Addr_val(address), String_val(contents),
         caml_string_length(contents));
  return Val_unit;
}

/* Makes the memory executable (and read-only), or writable. */
CAMLprim value camljit_protect(value address, value size, value executable)
{
  DWORD old;
  DWORD prot = Bool_val(executable) ? PAGE_EXECUTE_READ : PAGE_READWRITE;
  if (!VirtualProtect(Addr_val(address), Long_val(size), prot, &old))
    camljit_fail_with_last_error("VirtualProtect");
  if (Bool_val(executable))
    FlushInstructionCache(GetCurrentProcess(), Addr_val(address),
                          Long_val(size));
  return Val_unit;
}

/* From flexdll.h (flexdll is linked in the programs by flexlink). */
extern void *flexdll_wdlopen(const wchar_t *file, int mode);
#define FLEXDLL_RTLD_NOEXEC 0x0002

/* flexdll_dlsym waits for the mutex of the units of flexdll, which is only
   created by the first flexdll_wdlopen of a file: before (if the process
   has not loaded a library with Dynlink yet), it is INVALID_HANDLE_VALUE,
   the pseudo-handle of the current process, and the wait never ends. A
   library already loaded (kernel32.dll), opened without relocation
   (FLEXDLL_RTLD_NOEXEC) nor global symbols, creates the mutex. */
static void camljit_init_flexdll(void)
{
  static int initialized = 0;
  if (!initialized) {
    flexdll_wdlopen(L"kernel32.dll", FLEXDLL_RTLD_NOEXEC);
    initialized = 1;
  }
}

/* The address of a symbol of the process, or 0. */
CAMLprim value camljit_dlsym(value name)
{
  HMODULE modules[1024];
  DWORD needed, i;
  void *p;
  camljit_init_flexdll();
  p = caml_globalsym(String_val(name));
  if (p != NULL)
    return Val_addr(p);
  if (!K32EnumProcessModules(GetCurrentProcess(), modules, sizeof(modules),
                             &needed))
    return Val_addr(NULL);
  if (needed > sizeof(modules))
    needed = sizeof(modules);
  for (i = 0; p == NULL && i < needed / sizeof(HMODULE); i++)
    p = (void *) GetProcAddress(modules[i], String_val(name));
  return Val_addr(p);
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

#include <Rts.h>
#include <assert.h>

STATIC_INLINE void checkSym(int sym) {
#ifdef DEBUG
  cassert(sym + genSymInc <= HS_INT_MAX);
#endif
}

static HsInt genSymCounter = 0;
static HsInt genSymInc = 1;

HsInt genSym() {
#if defined(THREADED_RTS)
  if (n_capabilities == 1) {
    genSymCounter += genSymInc;
    checkSym(genSymCounter);
    return genSymCounter;
  } else {
    HsInt n = atomic_inc((StgWord*) &genSymCounter, genSymInc);
    checkSym(n);
    return n;
  }
#else
  genSymCounter += genSymInc;
  checkSym(genSymCounter);
  return genSymCounter;
#endif
}

void initGenSym(HsInt counter, HsInt inc) {
  genSymCounter = counter;
  genSymInc = inc;
}

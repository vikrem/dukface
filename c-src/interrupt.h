#ifndef INTERRUPT_H
#define INTERRUPT_H

#include "duktape.h"

int enable_interrupt(void);
void disable_interrupt(void);
void force_interrupt(void);
void install_handler(void);

void duktape_fatal_handler(void*, const char*);
duk_bool_t dukTimeoutCheck(void* udata);

#endif

#ifndef INTERRUPT_H
#define INTERRUPT_H

int enable_interrupt(void);
void disable_interrupt(void);
void force_interrupt(void);
void install_handler(void);

void duktape_fatal_handler(void*, const char*);


#endif

#ifndef INTERRUPT_H
#define INTERRUPT_H

void sigpipe_handle(int x);
void install_handler(void);
int interrupted(void);


#endif

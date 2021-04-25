// +++++++ ONLY MODIFY BELOW LINE 39 BELOW
#include "types.h"
#include "defs.h"
#include <stdio.h>
#include "proc.h"

#define NCPU 1

struct cpu cpus[NCPU];
int ncpu;

void printstate(enum procstate pstate){ // DO NOT MODIFY
  switch(pstate) {
    case UNUSED   : printf("UNUSED");   break;
    case EMBRYO   : printf("EMBRYO");   break;
    case SLEEPING : printf("SLEEPING"); break;
    case RUNNABLE : printf("RUNNABLE"); break;
    case RUNNING  : printf("RUNNING");  break;
    case ZOMBIE   : printf("ZOMBIE");   break;
    default       : printf("????????");
  }
}

// Per-CPU process scheduler.
// Each CPU calls scheduler() after setting itself up.
// Scheduler never returns.  It loops, doing:
//  - choose a process to run
//  - swtch to start running that process
//  - eventually that process transfers control
//      via swtch back to the scheduler.

// local to scheduler in xv6, but here we need them to persist outside,
// because while xv6 runs scheduler as a "coroutine" via swtch,
// here swtch is just a regular procedure call.
struct proc *p;
struct cpu *c = cpus;

//  Add in any global variables and functions you need below.
// +++++++ ONLY MODIFY BELOW THIS LINE ++++++++++++++++++++++

void scheduler(void){
  int runnableFound; // DO NOT MODIFY/DELETE
  c->proc = 0;

  // Initialize the run counters for each process to 0
  int procRunCounters[NPROC];
  for (int i = 0; i < NPROC; i++)
    procRunCounters[i] = 0;

  // The initial active process is the last one because the scheduler will choose
  // the first active process via round-robin resulting in index 0 being chosen
  int activeProcIndex = NPROC - 1;

  runnableFound = 1; // Force one pass over ptable
  while(runnableFound){ // DO NOT MODIFY
    runnableFound = 0; // DO NOT MODIFY
    // sti();

    // -1 is used to always mark the first runnable process found
    // An alternative is to use 'unsigned min = -1'
    int minProcRunCount = -1;

    // Iterate over every process, starting from the process after the lastest
    // active process and stopping once we've reached the lastest active process
    // acquire(&ptable.lock);
    int lastActiveProcIndex = activeProcIndex;
    for(int offset = 1; offset <= NPROC; offset++){
      int procIndex = (lastActiveProcIndex + offset) % NPROC;

      // Ignore unrunnable processes
      if(ptable.proc[procIndex].state != RUNNABLE)
        continue;
      runnableFound = 1; // DO NOT MODIFY/DELETE/BYPASS

      // Mark this process as the next active process if it's the least run process
      // we have seen so far while iterating over every runnable process
      // (note we always mark this process if it's the first we've seen)
      int procRunCount = procRunCounters[procIndex];
      if(procRunCount < minProcRunCount || minProcRunCount == -1){
        minProcRunCount = procRunCount;
        activeProcIndex = procIndex;
      }
    }

    // Only execute the marked process if a runnable process was actually found
    if(runnableFound){
      procRunCounters[activeProcIndex]++;
      p = ptable.proc + activeProcIndex;
      c->proc = p;
      //switchuvm(p);
      p->state = RUNNING;
      swtch(p);
      //switchkvm();
      c->proc = 0;
    }

    // release(&ptable.lock);
  }
  printf("No RUNNABLE process!\n");
}

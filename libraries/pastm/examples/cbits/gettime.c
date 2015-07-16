/*
 * Get the current time.  Apparently Mac OS X doesn't
 * implement clock_gettime, so instead I am using a workaround
 * taken from the following sources
 * github gist: https://gist.github.com/jbenet/1087739
 * stack overflow answer: http://stackoverflow.com/questions/5167269/clock-gettime-alternative-in-mac-os-x
 */

#include <time.h>
#include <sys/time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

double hs_gettime(){
  struct timespec ts;
  
#ifdef __MACH__  // OS X does not have clock_gettime, use clock_get_time
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts.tv_sec = mts.tv_sec;
  ts.tv_nsec = mts.tv_nsec;
#else
  clock_gettime(CLOCK_REALTIME, &ts);
#endif

  return ts.tv_sec + ts.tv_nsec * 1e-9;
    
}


/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#ifndef _INCLUDED_LISP_STARTUP_OPTIONS_H_
#define _INCLUDED_LISP_STARTUP_OPTIONS_H_

#include "os.h"

/* Staging area for (most of) the things startup gets from defaults,
 * saved executable settings, command-line options, and file system
 * searches. Any setting that needs to live past initialize_lisp()
 * gets copied elsewhere. */
struct lisp_runtime_options {
    bool noinform;
    char merge_core_pages; /* 3 states */
    bool disable_ldb;
    bool lose_on_corruption;
    /* Nothing in the system dispatches on this; it's just for
     * print_lisp_startup_options(). */
    bool saved_executable;
    /* 0 if none, 1 if the executable doesn't accept runtime
     * options, 2 if the executable does. */
    char embedded_options;
    os_vm_size_t dynamic_space_size;
    os_vm_size_t control_stack_size;
    int tls_limit; /* Note: not dynamic_values_bytes */
    char *core;
    /* Maybe this record should have members for the runtime and
     * homedir paths. Dunno. */
#ifdef LISP_FEATURE_WIN32
    wchar_t
#else
    char
#endif
    **posix_argv;
    /* We don't need separate fields to record flags the
     * not-saved-executable argv parser has special cases for: --help,
     * --version, --debug-environment, --script. */
};

#endif

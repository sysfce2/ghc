/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2021
 *
 * Support for mapping info table pointers to source locations
 *
 * ---------------------------------------------------------------------------*/


#include "rts/PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Hash.h"
#include "IPE.h"
#include "Printer.h"
#include "Profiling.h"
#include "RtsUtils.h"

#include <fs_rts.h>
#include <string.h>

#if defined(TRACING)
#include "Trace.h"
#endif

/*
Note [The Info Table Provenance Entry (IPE) Map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IPEs are stored in a hash map from info table address (pointer) to IPE. This
ensures cheap lookup and traversal.

Unfortunately, inserting into the hash map is relatively expensive. To keep
startup times low, there's a temporary data structure that is optimized for
collecting IPE lists on registration.

It's a singly linked list of IPE list buffers. Each buffer contains space for
126 IPE lists. This number is a bit arbitrary, but leaves a few bytes so that
the whole structure might fit into 1024 bytes.

On registering a new IPE list, there are three cases:

- It's the first entry at all: Allocate a new IpeBufferListNode and make it the
  buffer's first entry.
- The current IpeBufferListNode has space in it's buffer: Add it to the buffer.
- The current IpeBufferListNode's buffer is full: Allocate a new one and link it
to the previous one, making this one the new current.

Building the hash map is done lazily, i.e. on first lookup or traversal. For
this all IPE lists of all IpeBufferListNode are traversed to insert all IPEs.

After the content of a IpeBufferListNode has been inserted, it's freed.
*/

static HashTable *ipeMap = NULL;

static IpeBufferListNode *ipeBufferList = NULL;

static Mutex ipeMapLock;

void initIpeMapLock(void) { initMutex(&ipeMapLock); }

void closeIpeMapLock(void) { closeMutex(&ipeMapLock); }

#if defined(TRACING)
static void traceIPEFromHashTable(void *data STG_UNUSED, StgWord key STG_UNUSED,
                           const void *value) {
    InfoProvEnt *ipe = (InfoProvEnt *)value;

    traceIPE(ipe->info, ipe->prov.table_name, ipe->prov.closure_desc,
             ipe->prov.ty_desc, ipe->prov.label, ipe->prov.module,
             ipe->prov.srcloc);
}
#endif

void dumpIPEToEventLog(void) {
#if defined(TRACING)
    ACQUIRE_LOCK(&ipeMapLock);

    IpeBufferListNode *cursor = ipeBufferList;
    while (cursor != NULL) {
        for (int i = 0; i < cursor->count; i++) {
            for (InfoProvEnt **ipeList = cursor->buffer[i]; *ipeList != NULL;
                 ipeList++) {
                InfoProvEnt *ipe = *ipeList;

                traceIPE(ipe->info, ipe->prov.table_name,
                         ipe->prov.closure_desc, ipe->prov.ty_desc,
                         ipe->prov.label, ipe->prov.module, ipe->prov.srcloc);
            }
        }

        cursor = cursor->next;
    }

    if (ipeMap != NULL) {
        mapHashTable(ipeMap, NULL, &traceIPEFromHashTable);
    }

    RELEASE_LOCK(&ipeMapLock);
#endif
    return;
}

/* Registering IPEs

Adds the ent_list to the temporary buffer structure described in
Note [The Info Table Provenance Entry (IPE) Map].

Statically initialized IPE lists are registered at startup by a C constructor
function generated by the compiler (CodeOutput.hs) in a *.c file for each
module.

A performance test for IPE registration and lookup can be found here:
https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5724#note_370806
*/
void registerInfoProvList(InfoProvEnt **ent_list) {
    // The list must be dereferenceable.
    ASSERT(ent_list[0] == NULL || ent_list[0] != NULL);

    // Ignore empty lists
    if (ent_list[0] == NULL) {
        return;
    }

    ACQUIRE_LOCK(&ipeMapLock);

    if (ipeBufferList == NULL) {
        ASSERT(ipeBufferList == NULL);

        ipeBufferList = stgMallocBytes(sizeof(IpeBufferListNode),
                                       "registerInfoProvList-firstNode");
        ipeBufferList->buffer[0] = ent_list;
        ipeBufferList->count = 1;
        ipeBufferList->next = NULL;
    } else {
        if (ipeBufferList->count < IPE_LIST_NODE_BUFFER_SIZE) {
            ipeBufferList->buffer[ipeBufferList->count] = ent_list;
            ipeBufferList->count = ipeBufferList->count + 1;

            ASSERT(ipeBufferList->next == NULL ||
                   ipeBufferList->next->count == IPE_LIST_NODE_BUFFER_SIZE);
        } else {
            IpeBufferListNode *newNode = stgMallocBytes(
                sizeof(IpeBufferListNode), "registerInfoProvList-nextNode");
            newNode->buffer[0] = ent_list;
            newNode->count = 1;
            newNode->next = ipeBufferList;
            ipeBufferList = newNode;

            ASSERT(ipeBufferList->next->count == IPE_LIST_NODE_BUFFER_SIZE);
        }
    }

    RELEASE_LOCK(&ipeMapLock);
}

InfoProvEnt *lookupIPE(const StgInfoTable *info) {
    updateIpeMap();
    return lookupHashTable(ipeMap, (StgWord)info);
}

void updateIpeMap() {
    // Check if there's any work at all. If not so, we can circumvent locking,
    // which decreases performance.
    if (ipeMap != NULL && ipeBufferList == NULL) {
        return;
    }

    ACQUIRE_LOCK(&ipeMapLock);

    if (ipeMap == NULL) {
        ipeMap = allocHashTable();
    }

    while (ipeBufferList != NULL) {
        ASSERT(ipeBufferList->next == NULL ||
               ipeBufferList->next->count == IPE_LIST_NODE_BUFFER_SIZE);
        ASSERT(ipeBufferList->count > 0 &&
               ipeBufferList->count <= IPE_LIST_NODE_BUFFER_SIZE);

        IpeBufferListNode *currentNode = ipeBufferList;

        for (int i = 0; i < currentNode->count; i++) {
            for (InfoProvEnt **ipeList = currentNode->buffer[i];
                 *ipeList != NULL; ipeList++) {
                insertHashTable(ipeMap, (StgWord)(*ipeList)->info, *ipeList);
            }
        }

        ipeBufferList = currentNode->next;
        stgFree(currentNode);
    }

    RELEASE_LOCK(&ipeMapLock);
}

#if defined(DEBUG)
static bool dumpOneInfoTable(void *user, StgWord key STG_UNUSED, const void *value) {
    FILE *f = (FILE *) user;
    const InfoProvEnt *n = (const InfoProvEnt *) value;
    const StgInfoTable *info = INFO_PTR_TO_STRUCT(n->info);
    fprintf(f, "{\"info\": %d", (uintptr_t) info);
    fprintf(f, ", \"name\": \"%s\"", n->prov.table_name);
    fprintf(f, ", \"module\": \"%s\"", n->prov.module);
    fprintf(f, ", \"srcloc\": \"%s\"", n->prov.srcloc);
    fprintf(f, ", \"type\": \"%s\"", info_type_by_ip(info));
    if (ip_THUNK(info)) {
        StgThunkInfoTable *thunk_info = itbl_to_thunk_itbl(info);
        if (thunk_info->i.srt) {
            fprintf(f, ", \"srt\": %d", (uintptr_t) GET_SRT(thunk_info));
        }
    } else if (ip_STACK_FRAME(info)) {
        StgRetInfoTable *ret_info = itbl_to_ret_itbl(info);
        if (ret_info->i.srt) {
            fprintf(f, ", \"srt\": %d", (uintptr_t) GET_SRT(ret_info));
        }
    } else if (ip_HNF(info)) {
        StgFunInfoTable *fun_info = itbl_to_fun_itbl(info);
        if (fun_info->i.srt) {
            fprintf(f, ", \"srt\": %d", (uintptr_t) GET_FUN_SRT(fun_info));
        }
    }
    fprintf(f, "}\n");
    return true;
}

void dumpAllInfoTables(char *file) {
    updateIpeMap();
    FILE *f = fopen(file, "w");
    iterHashTable(ipeMap, f, dumpOneInfoTable);
    fclose(f);
}
#endif

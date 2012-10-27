#include <windows.h>

typedef struct _Pending {
    OVERLAPPED  Overlapped;
    void       *UserData;
} Pending;

#define toPending(overlappedPtr) \
    CONTAINING_RECORD(overlappedPtr, Pending, Overlapped);

#define fromPending(pendingPtr) \
    (&(pendingPtr)->Overlapped)

OVERLAPPED *c_iocp_new_overlapped(UINT64 offset, void *userdata)
{
    Pending *p = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*p));
    if (p == NULL) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    } else {
        p->Overlapped.Offset     = (DWORD) offset;
        p->Overlapped.OffsetHigh = (DWORD) (offset >> 32);
        p->UserData              = userdata;
        return fromPending(p);
    }
}

static void *finish_overlapped(OVERLAPPED *o)
{
    void *userdata = toPending(o)->UserData;
    HeapFree(GetProcessHeap(), 0, o);
    return userdata;
}

/*
 * Return values:
 *  TRUE:  Completion (successful or otherwise) was received.
 *  FALSE: GetQueuedCompletionStatus failed or timed out.
 *         numBytes_out and userdata_out are undefined.
 */
BOOL c_iocp_get_next_completion(HANDLE iocp, DWORD timeout,
    DWORD *numBytes_out, DWORD *err_out, void **userdata_out)
{
    OVERLAPPED *overlapped = NULL;
    ULONG_PTR completionKey = 0;
    BOOL ok;

    *numBytes_out = 0;
    ok = GetQueuedCompletionStatus(port, numBytes_out, &completionKey,
            &overlapped, timeout);
    *err_out = ok ? ERROR_SUCCESS : GetLastError();
    if (overlapped == NULL)
        return FALSE;
    *userdata_out = finish_overlapped(overlapped);
    return TRUE;
}

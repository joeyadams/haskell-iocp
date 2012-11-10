#include <windows.h>

typedef ULONGLONG (WINAPI *GetTickCount64_t)(void);

GetTickCount64_t iocp_load_GetTickCount64(void)
{
    return (GetTickCount64_t)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                       "GetTickCount64");
}

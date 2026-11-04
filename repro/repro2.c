/* C-only repro with a persistent worker thread (like GHC's non-threaded
   async I/O): the worker starts a blocking recv() right when signalled,
   while the main thread closes the peer immediately. */
#include <winsock2.h>
#include <afunix.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

static SOCKET target;
static HANDLE go, done;
static volatile int result;

static DWORD WINAPI worker(LPVOID p) {
    char buf[4096];
    for (;;) {
        WaitForSingleObject(go, INFINITE);
        int n;
        while ((n = recv(target, buf, sizeof buf, 0)) > 0) ;
        result = n;
        SetEvent(done);
    }
    return 0;
}

static void pair(SOCKET *cli, SOCKET *srv) {
    char dir[MAX_PATH], path[MAX_PATH];
    struct sockaddr_un a = {0};
    GetTempPathA(MAX_PATH, dir);
    GetTempFileNameA(dir, "pr", 0, path);
    DeleteFileA(path);
    a.sun_family = AF_UNIX;
    strncpy(a.sun_path, path, sizeof a.sun_path - 1);
    SOCKET l = socket(AF_UNIX, SOCK_STREAM, 0);
    if (bind(l, (struct sockaddr *)&a, sizeof a) || listen(l, 10)) { printf("bind/listen %d\n", WSAGetLastError()); exit(1); }
    *cli = socket(AF_UNIX, SOCK_STREAM, 0);
    if (connect(*cli, (struct sockaddr *)&a, sizeof a)) { printf("connect %d\n", WSAGetLastError()); exit(1); }
    *srv = accept(l, NULL, NULL);
    closesocket(l);
    DeleteFileA(path);
}

int main(int argc, char **argv) {
    WSADATA w;
    int n = argc > 1 ? atoi(argv[1]) : 1000;
    int spin = argc > 2 ? atoi(argv[2]) : 0, sendone = argc > 3, hangs = 0;
    WSAStartup(MAKEWORD(2, 2), &w);
    go = CreateEvent(NULL, FALSE, FALSE, NULL);
    done = CreateEvent(NULL, FALSE, FALSE, NULL);
    CreateThread(NULL, 0, worker, NULL, 0, NULL);
    for (int i = 0; i < n; i++) {
        SOCKET cli, srv;
        pair(&cli, &srv);
        target = srv;
        SetEvent(go);
        for (volatile int k = 0; k < spin; k++) ;
        if (sendone) send(cli, "x", 1, 0);
        closesocket(cli);
        if (WaitForSingleObject(done, 2000) == WAIT_TIMEOUT) {
            hangs++;
            printf("  hang at iteration %d\n", i);
            closesocket(srv); /* should abort the blocked recv */
            WaitForSingleObject(done, INFINITE);
        } else {
            closesocket(srv);
        }
    }
    printf("RESULT C2 spin=%d %s: %d hangs / %d iterations\n", spin, sendone ? "sendone" : "nosend", hangs, n);
    return 0;
}

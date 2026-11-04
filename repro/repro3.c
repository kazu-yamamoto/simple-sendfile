/* Variant of repro2.c: socket family (unix|tcp) x receive method
   (blocking|event: overlapped WSARecv + event|iocp: overlapped WSARecv + IOCP). */
#include <winsock2.h>
#include <ws2tcpip.h>
#include <afunix.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static SOCKET target;
static HANDLE go, done, port;
static const char *method;

static int recv_once(char *buf, int len) {
    if (!strcmp(method, "blocking")) return recv(target, buf, len, 0);
    WSABUF wb = { (ULONG)len, buf };
    DWORD bytes = 0, flags = 0;
    WSAOVERLAPPED ov = {0};
    int iocp = !strcmp(method, "iocp");
    if (!iocp) ov.hEvent = (HANDLE)((ULONG_PTR)CreateEvent(NULL, TRUE, FALSE, NULL) | 1);
    int r = WSARecv(target, &wb, 1, &bytes, &flags, &ov, NULL);
    int res;
    if (r == 0 && !iocp) {
        res = (int)bytes;
    } else if (r != 0 && WSAGetLastError() != WSA_IO_PENDING) {
        res = -1;
    } else if (iocp) {
        /* With IOCP, even immediate completions are queued. */
        ULONG_PTR key; LPOVERLAPPED pov;
        BOOL ok = GetQueuedCompletionStatus(port, &bytes, &key, &pov, INFINITE);
        res = ok ? (int)bytes : -1;
    } else {
        WaitForSingleObject((HANDLE)((ULONG_PTR)ov.hEvent & ~(ULONG_PTR)1), INFINITE);
        res = WSAGetOverlappedResult(target, &ov, &bytes, FALSE, &flags) ? (int)bytes : -1;
    }
    if (!iocp) CloseHandle((HANDLE)((ULONG_PTR)ov.hEvent & ~(ULONG_PTR)1));
    return res;
}

static DWORD WINAPI worker(LPVOID p) {
    char buf[4096];
    for (;;) {
        WaitForSingleObject(go, INFINITE);
        while (recv_once(buf, sizeof buf) > 0) ;
        SetEvent(done);
    }
    return 0;
}

static void pair_unix(SOCKET *cli, SOCKET *srv) {
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

static void pair_tcp(SOCKET *cli, SOCKET *srv) {
    struct sockaddr_in a = {0};
    int alen = sizeof a;
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    SOCKET l = socket(AF_INET, SOCK_STREAM, 0);
    if (bind(l, (struct sockaddr *)&a, sizeof a) || listen(l, 10)) { printf("bind/listen %d\n", WSAGetLastError()); exit(1); }
    getsockname(l, (struct sockaddr *)&a, &alen);
    *cli = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(*cli, (struct sockaddr *)&a, sizeof a)) { printf("connect %d\n", WSAGetLastError()); exit(1); }
    *srv = accept(l, NULL, NULL);
    closesocket(l);
}

int main(int argc, char **argv) {
    if (argc < 5) { printf("usage: repro3 N SPIN unix|tcp blocking|event|iocp [sendone]\n"); return 2; }
    WSADATA w;
    int n = atoi(argv[1]), spin = atoi(argv[2]), sendone = argc > 5, hangs = 0;
    const char *family = argv[3];
    method = argv[4];
    WSAStartup(MAKEWORD(2, 2), &w);
    go = CreateEvent(NULL, FALSE, FALSE, NULL);
    done = CreateEvent(NULL, FALSE, FALSE, NULL);
    port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
    CreateThread(NULL, 0, worker, NULL, 0, NULL);
    for (int i = 0; i < n; i++) {
        SOCKET cli, srv;
        if (!strcmp(family, "tcp")) pair_tcp(&cli, &srv); else pair_unix(&cli, &srv);
        if (!strcmp(method, "iocp")) CreateIoCompletionPort((HANDLE)srv, port, 0, 0);
        target = srv;
        SetEvent(go);
        for (volatile int k = 0; k < spin; k++) ;
        if (sendone) send(cli, "x", 1, 0);
        closesocket(cli);
        if (WaitForSingleObject(done, 2000) == WAIT_TIMEOUT) {
            hangs++;
            closesocket(srv); /* aborts the pending receive */
            WaitForSingleObject(done, INFINITE);
        } else {
            closesocket(srv);
        }
    }
    printf("RESULT %-4s %-8s spin=%-6d %-7s: %d hangs / %d iterations\n",
           family, method, spin, sendone ? "sendone" : "nosend", hangs, n);
    return 0;
}

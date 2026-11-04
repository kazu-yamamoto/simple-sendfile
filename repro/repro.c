/* C-only repro: blocking recv() in a worker thread on an AF_UNIX pair
   (built like network's socketPair on Windows) while the peer is closed. */
#include <winsock2.h>
#include <afunix.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

static DWORD WINAPI reader(LPVOID p) {
    SOCKET s = *(SOCKET *)p;
    char buf[4096];
    int n;
    while ((n = recv(s, buf, sizeof buf, 0)) > 0) ;
    return n == 0 ? 0 : 1;
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
    int n = argc > 1 ? atoi(argv[1]) : 1000, sendone = argc > 2, hangs = 0;
    WSAStartup(MAKEWORD(2, 2), &w);
    for (int i = 0; i < n; i++) {
        SOCKET cli, srv;
        pair(&cli, &srv);
        HANDLE t = CreateThread(NULL, 0, reader, &srv, 0, NULL);
        if (sendone) send(cli, "x", 1, 0);
        closesocket(cli);
        if (WaitForSingleObject(t, 2000) == WAIT_TIMEOUT) {
            hangs++;
            printf("  hang at iteration %d\n", i);
            closesocket(srv);
            WaitForSingleObject(t, INFINITE);
        } else {
            closesocket(srv);
        }
        CloseHandle(t);
    }
    printf("RESULT C %s: %d hangs / %d iterations\n", sendone ? "sendone" : "nosend", hangs, n);
    return 0;
}

import os
import socket
import struct
import sys

BASEDIR = os.environ['RUNNER_BASEDIR']


def daemonize() -> None:
    pid = os.fork()
    if pid > 0:
        os._exit(0)
    os.setsid()
    pid = os.fork()
    if pid > 0:
        os._exit(0)


def recvall(sock: socket.socket, size: int) -> bytes:
    data = b""
    while len(data) < size:
        data += sock.recv(size - len(data))
    return data


def recvuntil(sock: socket.socket, char: bytes) -> bytes:
    result = b""
    while True:
        buf = recvall(sock, 1)
        if buf == char:
            break
        result += buf
    return result


def getchunk(sock: socket.socket) -> bytes:
    szlen = struct.calcsize('!Q')
    size = struct.unpack('!Q', recvall(sock, szlen))[0]
    data = recvall(sock, size)
    return data


def shesc(val: bytes) -> bytes:
    return b"'" + val.replace(b"'", b"'\\''") + b"'"


def handle(host: socket.socket, guest: socket.socket) -> None:
    cmd = getchunk(host)
    wrapper = b'errs="$(mktemp)"; '
    wrapper += b'output="$(sh -c ' + shesc(cmd) + b' 2> "$errs"; echo -n x)"; '
    wrapper += b'retcode=$?; '
    wrapper += b'errors="$(cat "$errs"; echo -n x)"; '
    wrapper += b'rm -f "$errs"; '
    wrapper += b'output="${output%x}"; '
    wrapper += b'errors="${errors%x}"; '
    wrapper += b'echo "${#output}:$output"; '
    wrapper += b'echo "${#errors}:$errors"; '
    wrapper += b'echo ",$retcode;"'
    guest.sendall(wrapper + b"\n")

    datalen = int(recvuntil(guest, b':'))
    output = recvall(guest, datalen)

    datalen = int(recvuntil(guest, b':'))
    errors = recvall(guest, datalen)

    recvuntil(guest, b',')
    retcode = recvuntil(guest, b';')
    recvall(guest, 1)

    enclen = struct.pack('!Q', len(output))
    encoded = enclen + output
    enclen = struct.pack('!Q', len(errors))
    encoded += enclen + errors
    encoded += struct.pack('!I', int(retcode))

    host.sendall(encoded)


if os.path.exists(os.path.join(BASEDIR, '.run-in-guest')):
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.connect(os.path.join(BASEDIR, '.run-in-guest'))
        cmd = b' '.join(shesc(os.fsencode(arg)) for arg in sys.argv[1:])
        client.sendall(struct.pack('!Q', len(cmd)) + cmd)
        output = getchunk(client)
        errors = getchunk(client)
        retcode = recvall(client, struct.calcsize('!I'))
        sys.stderr.buffer.write(errors)
        sys.stdout.buffer.write(output)
        raise SystemExit(struct.unpack('!I', retcode)[0])
else:
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as guest, \
         socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as host:
        guest.bind(os.path.join(BASEDIR, 'shell'))
        guest.listen()
        host.bind(os.path.join(BASEDIR, '.run-in-guest'))
        host.listen()
        daemonize()
        with guest.accept()[0] as shell:
            recvuntil(shell, b'\n')
            shell.sendall(b'PS2=\n')
            while True:
                with host.accept()[0] as hostsock:
                    handle(hostsock, shell)

#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3Packages.python gnugrep nix --argstr # noqa

# This script is to fetch a list of possible canaries for function calls
# arising from our patches. The reason is that whenever we change function
# arguments, the upstream project might add additional calls to these functions
# in future versions.

import json
import os
import re
import subprocess
import sys

FUNCALL = re.compile(r'([a-zA-Z0-9_]+)\s*\((.*?)\)(?!.*{)')
RESERVED = ["if", "while", "for"]

BASEDIR = os.path.dirname(os.path.realpath(__file__))
PATCHDIR = os.path.join(BASEDIR, 'patches')


def get_existing_canaries():
    nixexpr = '((import <nixpkgs> {}).callPackage ./shabitica.nix {})'
    nixexpr += '.source.functionCanaries'
    cmd = ['nix-instantiate', '--eval', '--json', '-E', nixexpr]
    output = subprocess.check_output(cmd, cwd=BASEDIR).decode()
    return set([c.split('(', 1)[0] for c in json.loads(output)])


def expand_args(args):
    return list(map(str.strip, args.split(',')))


def find_funcalls(line):
    funcalls = filter(lambda x: x[0] not in RESERVED, FUNCALL.findall(line))
    return [tuple([name] + expand_args(args)) for name, args in funcalls
            if '=' not in args]


def fetch_canaries(old_funcalls, new_funcalls):
    oldargs = {fc[0]: len(fc[1:]) for fc in old_funcalls}
    newargs = {fc[0]: len(fc[1:]) for fc in new_funcalls}

    common_funs = set(oldargs.keys()) & set(newargs.keys())
    diffargs = [fun for fun in common_funs if oldargs[fun] != newargs[fun]]

    return {fun: (oldargs[fun], newargs[fun]) for fun in diffargs}


def suggest_canary(fun, args):
    if args == 0:
        argregex = "()"
    elif args == 1:
        argregex = "[^)]"
    elif args == 2:
        argregex = "[^,)]*,"
    else:
        argregex = "[^,]*," * (args - 2) + "[^,)]*,"

    return fun + "(" + argregex


def mkcolor(value, color):
    return chr(27) + "[" + str(color) + "m" + str(value) + chr(27) + "[m"


def show_canary(path, diff_del, diff_add, fun, argnum_old, argnum_new, sugg):
    patchname = os.path.basename(path)
    msg = "Function {} has changed from {} arguments to {} arguments.\n"
    out = msg.format(mkcolor(fun, '1;33'),
                     mkcolor(argnum_old, '1;31'),
                     mkcolor(argnum_new, '1;32'))
    pathcolor = mkcolor(patchname, 35)
    msg = "Originating patch file is {}, diff content is:\n"
    out += msg.format(pathcolor)
    out += mkcolor(diff_del.rstrip(), 31) + "\n"
    out += mkcolor(diff_add.rstrip(), 32) + "\n"
    out += "Suggested canary is: {}\n\n".format(mkcolor(sugg, '1;36'))
    return out


if __name__ == '__main__':
    existing = get_existing_canaries()

    canaries = {}
    for patchname in os.listdir(PATCHDIR):
        path = os.path.join(PATCHDIR, patchname)
        with open(path, 'r') as patchfile:
            old_funcalls = []
            oldline = None
            for line in patchfile:
                if line.startswith('-'):
                    old_funcalls = find_funcalls(line)
                    oldline = line
                    continue

                if oldline is None:
                    continue

                if line.startswith('+') and len(old_funcalls) > 0:
                    new_funcalls = find_funcalls(line)
                    new_canaries = fetch_canaries(old_funcalls, new_funcalls)
                    for fun, (old, new) in new_canaries.items():
                        canaries[fun] = (path, oldline, line, fun, old, new,
                                         suggest_canary(fun, old))
                    old_funcalls = []

                oldline = None

    for fun in existing:
        if fun in canaries:
            del canaries[fun]

    output = []
    for path, diffdel, diffadd, fun, argold, argnew, sugg in canaries.values():
        out = show_canary(path, diffdel, diffadd, fun, argold, argnew, sugg)
        output.append(out)
    sys.stdout.write(''.join(output))

    if len(output) > 0:
        sys.stdout.write(mkcolor("All suggestions:", '1;35') + "\n")
        for vals in canaries.values():
            quot = mkcolor('"', '1;30')
            out = "  " + quot + mkcolor(vals[6], '1;34') + quot + "\n"
            sys.stdout.write(out)

#!/usr/bin/env nix-shell
#!nix-shell -i python2 -p python nix nodePackages.node2nix nixops --argstr # noqa
import os
import json
import tempfile
import itertools
import subprocess

from nixops import nix_expr

BASEDIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), '..')
OUTPUT_DIR = os.path.join(BASEDIR, 'generated')
COMPOSITION_FILENAME = 'all-deps.nix'
COMPOSITION_FILE = os.path.join(OUTPUT_DIR, COMPOSITION_FILENAME)


def get_source_path():
    nixexpr = '((import <nixpkgs> {}).callPackage ./shabitica.nix {}).source'
    cmd = ['nix-build', '--no-out-link', '-E', nixexpr]
    return subprocess.check_output(cmd, cwd=BASEDIR).decode().strip()


def get_package_description():
    srcdir = get_source_path()
    with open(os.path.join(srcdir, "package.json"), 'r') as fp:
        return json.load(fp)


def deps2list(val):
    if isinstance(val, dict):
        return [{k: v} for k, v in val.items()]
    else:
        return val


def generate(depmap):
    flattened = list(itertools.chain(*[deps2list(d) for d in depmap.values()]))
    with tempfile.NamedTemporaryFile() as fp:
        json.dump(flattened, fp)
        fp.flush()
        cmd = [
            'node2nix', '--nodejs-10',
            '-e', os.path.join(OUTPUT_DIR, 'node-env.nix'),
            '-i', fp.name,
            '-o', os.path.join(OUTPUT_DIR, 'node-packages.nix'),
            '-c', COMPOSITION_FILE
        ]
        subprocess.check_call(cmd, cwd=BASEDIR)


def generate_depmap_expr(depmap):
    args = [
        "pkgs ? import <nixpkgs> { inherit system; }",
        "system ? builtins.currentSystem",
        "nodejs ? pkgs.nodejs"
    ]
    fun_head = '{ ' + '\n, '.join(args) + '\n}:\n\n'

    alldeps = "import ./" + COMPOSITION_FILENAME
    alldeps += " { inherit pkgs system nodejs; }"
    letexpr = "let\n  nodePackages = {};\nin ".format(alldeps)

    return fun_head + letexpr + nix_expr.py2nix(depmap)


def ref_nodepkg(attr):
    return nix_expr.RawValue('nodePackages.' + nix_expr.py2nix(attr))


def deps2nix(deps):
    out = {}
    if isinstance(deps, dict):
        for name, version in deps.items():
            out[name] = ref_nodepkg(name + "-" + version)
    else:
        for name in deps:
            out[name] = ref_nodepkg(name)
    return out


def generate_depmap(depmap):
    deps = {cat: deps2nix(deps) for cat, deps in depmap.items()}
    expr = generate_depmap_expr(deps)
    with open(os.path.join(OUTPUT_DIR, 'default.nix'), 'w') as fp:
        fp.write(expr + "\n")


def main():
    pkgdesc = get_package_description()
    depmap = {
        'main': pkgdesc['dependencies'],
        'dev': pkgdesc['devDependencies'],
        'extra': ['ttf2svg', 'ttf2eot', 'ttf2woff'],
    }
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
    generate_depmap(depmap)
    generate(depmap)


if __name__ == '__main__':
    main()

********************************
Helper utilities
********************************

build-vm.nix
````````````

Nix expression to build a VM for testing. The resulting file is the script to
run the VM, which forwards the web server port to the host and makes it
available via ``http://localhost:3000/``.

The VM also has a Postfix mail service running, which redirects **all** mails
to the mail box of the ``root`` user. A patched version of the `mutt`_ e-mail
client is also available, so just invoking ``mutt`` will give you access to all
mails that were sent out by Shabitica.

.. warning:: This is **NOT** suitable for running in production and uses
             insecure defaults.

find-canaries.py
````````````````

Searches for function argument canaries in the patches.

These canaries are used to ensure that whenever we add/remove function
arguments via a patch, we will get a build failure if one of these functions
are called with a different arity/amount of args.

kill-files.sh
`````````````

The source derivation removes a bunch of files (mainly for 3rd-party services)
in order to make sure they're not referenced anywhere.

When working on patches in a Git clone of `Habitica`_, it sometimes is useful
to remove these files in the Git working directory as well in order to make
sure that the resulting patch doesn't conflict with the source derivation.

This command does exactly that when executed while the Git working directory is
the current working directory.

update-patches.sh
`````````````````

Gathers all the commits from the latest tagged version to the current Git HEAD
and writes them as patches in the patches/ subdirectory. Only commits that have
a "Filename:" tag are recognized.

update-deps.sh
``````````````

Update all the node dependencies to their latest versions. Note that package
locks are ignored, so it might not *always* be a good idea to update this
regularily.

.. _Habitica: https://habitica.com/
.. _mutt: http://www.mutt.org/

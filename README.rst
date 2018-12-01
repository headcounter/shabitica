********************************
Shabitica - Self-hosted Habitica
********************************

This is a fork of `Habitica`_, a habit tracker which treats your goals like a
Role Playing Game.

The main target OS for this project is `NixOS`_ and while it is possible to
run it on other distributions/operating systems running Nix it will be harder
to set up.

Differences from the upstream project
-------------------------------------

Primarily the goal is to track the upstream project as much as possible and
contribute back any changes that might be useful for Habitica overall.

However, there are a few fundamental differences we can't (easily) get
upstream:

Do not use any 3rd-party services
`````````````````````````````````

The upstream project has a lot of them, ranging from payment providers to
analytics, social integration and more. In order to make sure we don't
accidentally introduce new 3rd-party services we have canaries in the source
build that check for certain services.

Sandboxing by default whenever possible
```````````````````````````````````````

We're running Habitica in a `network namespace`_ with only the loopback
interface available. Communication with services such as the mailer or the
database is done solely via UNIX Domain Sockets.

Instead of the MongoDB NixOS service we run our own MongoDB service patched to
support systemd startup notifications and only connecting via UNIX Domain
Sockets as well.

This is to make sure we have a very low attack surface and ideally compromising
the service should only affect Habitica itself and not other services running
on the machine.

Invite-only by default
``````````````````````

Only allow to publicly register the first user. Attempts to register another
user will only work via invitation link.

Free subscriptions for all users
````````````````````````````````

We have stripped out all payment systems, so getting gems can be done by
converting Gold into Gems. All features requiring a subscription are also
unlocked for every user.

Use a custom mailer daemon
``````````````````````````

Habitica uses `Mandrill`_ for sending emails and the templates for these emails
are not publicly available. So we do have our own mailer daemon, which
replicates the API endpoints required to send it out. Right now the daemon only
supports sending via the `sendmail`_ binary.

No censorship of messages
`````````````````````````

Word banning is not only error prone but also doesn't make sense on private
servers (if the person you've invited does offend you, why did you invite
them?) and can be easily circumvented anyway (eg. using unicode).

No support for the mobile apps
``````````````````````````````

Maintaining forks for these apps would be way too much work and would also make
it difficult to run without 3rd-party services, so the long term goal is to
make the browser client less painful on mobile phones.

Getting started on NixOS
------------------------

Simply adding the path of the source to the ``imports`` list of your `NixOS
configuration`_ (typically ``/etc/nixos/configuration.nix``) will start a local
instance.

While the source tree can be obtained by cloning this repository (or using
``fetchGit``/``fetchTarball``), it's recommended to use one of the Hydra
channels, which only contain the latest *tested* version of the source code.

If you're on NixOS 18.03, you can add the channel via:

.. code-block:: sh-session

  # nix-channel --add https://headcounter.org/hydra/channel/custom/shabitica/nixos-18.03/shabitica
  # nix-channel --update shabitica

If you're using NixOS Unstable, you can add it like this:

.. code-block:: sh-session

  # nix-channel --add https://headcounter.org/hydra/channel/custom/shabitica/nixos-unstable/shabitica
  # nix-channel --update shabitica

Example configuration
`````````````````````

.. code-block:: nix

  {
    imports = [ <shabitica> ];
    shabitica.hostName = "shabitica.example.org";
    shabitica.adminMailAddress = "root@example.org";
    shabitica.senderMailAddress = "shabitica@example.org";
  }

This configures Shabitica to run on ``shabitica.example.org`` with an `NGINX
reverse proxy`_. If you don't provide any options, it will run on your local
machine only.

Please take a look at latest `manual`_ for all of the available options.

Development/Testing
-------------------

There is a Nix expression in a file called ``build-vm.nix``, which builts a
script to boot up a local VM with a fully running Shabitica instance, useful
for testing and development.

The NGINX web server listening on port 3000 is forwarded to the host, so you
can access Shabitica by pointing your browser at http://localhost:3000/.

All mails sent within than VM (to any address) are routed to the root user's
mailbox which can be viewed by simply running ``mutt`` inside the VM. This is
particularly useful if you want to add another user, so you can send an
invitation mail and get the link via ``mutt``.

This should also work on other distributions running `Nix`_.

Helper utilities
----------------

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
sure that the resulting patch doesn't conflicts with the source derivation.

This command does exactly that when executed while the Git working is the
current working directory.

update-patches.sh
`````````````````

Gathers all the commits from the latest tagged version to the current Git HEAD
and writes them as patches in patches/ subdirectory. Only commits that have a
"Filename:" tag are recognized.

update-deps.sh
``````````````

Update all the node dependencies to their latest versions. Note that package
locks are ignored, so it might not *always* be a good idea to update this
regularily.

Ideas
-----

* Allow to build the client as an Electron app, and allow to use end-to-end
  encryption for tasks.

.. _Habitica: https://habitica.com/
.. _Hydra jobsets: https://headcounter.org/hydra/project/shabitica
.. _Mandrill: https://www.mandrill.com/
.. _NGINX reverse proxy: https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/
.. _Nix: https://nixos.org/nix/
.. _NixOS configuration: https://nixos.org/nixos/manual/index.html#sec-configuration-file
.. _NixOS: https://nixos.org/
.. _manual: https://headcounter.org/hydra/job/shabitica/nixos-unstable/manual/latest/download
.. _network namespace: https://en.wikipedia.org/wiki/Linux_namespaces#Network_(net)
.. _sendmail: http://www.postfix.org/sendmail.1.html

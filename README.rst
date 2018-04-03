*******************
Habitica Standalone
*******************

This is a fork of `Habitica`_, a habit tracker which treats your goals like a
Role Playing Game.

The primary goal of this fork is to enable users to run their own private
instances.

Most of the plumbing in this repository is implemented in the `Nix`_ language
and package manager.

Current status
--------------

Currently it only runs on `NixOS`_, but a Docker image or even native
distribution packages might be on the roadmap, who knows?

Some of the patches are a bit ugly and need improvement and/or generalization,
so in the best possible case they can even be submitted upstream.

Also, the mobile Apps are currently not supported (and stripped out), but
making the client more mobile-friendly could be a viable alternative.

Getting started on NixOS
------------------------

The easiest way is to just add the channel by issuing:

.. code-block:: sh-session

  # nix-channel --add https://headcounter.org/hydra/channel/custom/headcounter/habitica/habitica
  # nix-channel --update habitica

You can add the service to your `NixOS configuration`_ (typically
``/etc/nixos/configuration.nix``) by just importing ``<habitica>``, like this:

.. code-block:: nix

  {
    imports = [ <habitica> ];
    habitica.hostName = "habitica.example.org";
    habitica.adminMailAddress = "root@example.org";
    habitica.senderMailAddress = "habitica@example.org";
  }

This configures `Habitica`_ to run on ``habitica.example.org`` with an `NGINX
reverse proxy`_. If you don't provide any options, it will run on your local
machine only.

Please take a look at latest `manual`_ for available options.

Project goals
-------------

* No external services whatsoever. When using the client, no external
  requests should be made, neither should the server do so (it uses a network
  namespace).
* Parts that require subscription should be accessible by users for free.
* Make it as easy as possible to spin up an instance on your server.
* Aim for private servers, typically with a small amount of users.

.. _Habitica: https://habitica.com/
.. _Nix: https://nixos.org/nix/
.. _NixOS configuration: https://nixos.org/nixos/manual/index.html#sec-configuration-file
.. _NixOS: https://nixos.org/
.. _manual: https://headcounter.org/hydra/job/headcounter/habitica/manual/latest/download
.. _NGINX reverse proxy: https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/

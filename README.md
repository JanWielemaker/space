# SWI-Prolog spatial reasoning pack (space)

This directory contains a [SWI-Prolog
pack](https://www.swi-prolog.org/pack/list) for spatial reasoning. It has
been developed by Willem van Hage and was originally distributed as part
of the SWI-Prolog source. Due to the relative small user base and
not-so-easy-to-install dependencies it has been removed from the core
sources.

## Installing dependencies

  - libgeos
  - libspatialindex

On recent Debian versions (_bullseye_ or _bookworm_) these may be installed using

    apt install libspatialindex-dev libgeos-dev libgeos++-dev libserd-dev

The pack has been built and tested with libgeos++-dev 3.11.1 and might
not work with other releases (the C++ interface is documented as being
"unstable"). The symptom of incompatibility is that the pack install
fails with C++ compilation errors.

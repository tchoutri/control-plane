# Control Plane [![CI-badge][CI-badge]][CI-url] ![simple-haskell][simple-haskell]

## Description

This is the repository for the system that I use as my own control plane. It serves as a monitoring system, job queues control panel,
notifications endpoint for webhooks, etc.

## Build 

The backend is implemented in Haskell. You will need the Nix package manager to handle external dependencies.

To build the backend without optimisations, run:

```bash
$ cabal build
```

or, to enable optimisations

```bash
$ cabal build -O2
```

## Run

You will need a PostgreSQL 12 database or higher.
Please configure the environment variables in the appropriate source file in `./deployment`
and source the file in your environment.

To launch the server, run 

```bash
$ cabal run exe:control-plane-server start
```

To launch the job runner, run

```bash
$ cabal run exe:control-plane-jobs 
```

You can start both of them in a tmux shell by running `start-tmux.sh`

While the former and the latter do not need to reside on the same machine, they both need access to the database server.

## Features

### Notifications

### Website monitoring

* Cards with the domain name, port, current status (reachable / unreacheable since \<timestamp\>)

[CI-url]: https://github.com/tchoutri/control-plane/actions
[CI-badge]: https://img.shields.io/github/workflow/status/tchoutri/control-plane/CI?style=flat-square
[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square

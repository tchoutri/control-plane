# Control Plane ![Simple Haskell][Simple Haskell] [![GitHub CI](https://github.com/tchoutri/control-plane/workflows/CI/badge.svg)][GitHub Action]

## Description

This is the repository for the system that I use as my own control plane. It serves as a monitoring system, job queues control panel,
notifications endpoint for webhooks, etc.

## Build 

The backend is implemented in Haskell. You will need the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) build tool
to build it, as well as the Nix package manager to handle external dependencies.

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
$ cabal exec -- control-plane-server start
```

## Feature map

## Notifications

* Accept webhooks to display notification toasts
* Host a section for Pushover notifications

## Website monitoring

* Cards with the domain name, port, current status (reachable / unreacheable since <timestamp>)

## Job queue

* Backed by `odd-jobs` + postgresql

[Simple Haskell]: https://www.simplehaskell.org/badges/badge2.svg
[GitHub Action]: https://github.com/tchoutri/control-plane/actions

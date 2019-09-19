# Getting started

## Viewing a basic animation

```console
$ git clone https://github.com/Lemmih/reanimate.git
$ cd reanimate/
$ stack build
$ stack ./examples/raster.hs
```

## Checking dependencies

Reanimate includes a run-time for optional dependencies.

```console
$ stack ./examples/raster.hs check
```

## Rendering movies

```console
$ stack ./examples/raster.hs render
```

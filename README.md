# Plugins for Nova framework

This repository contains a collection of plugins that can be used with [Nova Framework](https://github.com/novaframework/nova).

## Markdown version

There's a markdown version of the index that can be found [here](index.md)

## Integration with rebar3_nova-plugin (Rebar plugin)

TBA

## Build a new index

Go into the nova_plugins directory and run `rebar3 shell`. Then call `nova_plugins:build_index("../index.json").` to generate a new index. The old one will be overwritten.

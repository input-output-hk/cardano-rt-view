# RTView Configuration

## Saved configuration

During the start, RTView initiates an interactive dialog with the user. Based on the given answers, it creates a configuration file (see an example below). This file will be automatically saved on your hard drive in the default config directory, depending on your platform. For example, on Linux, it's `$HOME/.config/`.

So, when you start RTView again, it will ask you:

```
Saved configuration file is found. Do you want to use it? <Y/N>
```

`Y` is a default option, so if you press `Y` or `Enter`, the saved configuration will be used. Otherwise, you will enter into the same interactive dialog again.

## Configuration file: example

This is the complete example of saved configuration file (taken from `NixOS 20.03`):

```
rotation: null
defaultBackends:
- KatipBK
setupBackends:
- KatipBK
- LogBufferBK
- TraceAcceptorBK
hasPrometheus: null
hasGraylog: null
hasGUI: null
traceForwardTo: null
traceAcceptAt:
- remoteAddr:
    tag: RemotePipe
    contents: /tmp/rt-view-pipes/node-1
  nodeName: node-1
- remoteAddr:
    tag: RemotePipe
    contents: /tmp/rt-view-pipes/node-2
  nodeName: node-2
- remoteAddr:
    tag: RemotePipe
    contents: /tmp/rt-view-pipes/node-3
  nodeName: node-3
defaultScribes:
- - StdoutSK
  - stdout
options:
  mapBackends:
    cardano-rt-view.acceptor:
    - LogBufferBK
    - kind: UserDefinedBK
      name: ErrorBufferBK
setupScribes:
- scMaxSev: Emergency
  scName: stdout
  scRotation: null
  scMinSev: Notice
  scKind: StdoutSK
  scFormat: ScText
  scPrivacy: ScPublic
hasEKG: null
forwardDelay: null
minSeverity: Info
```

## Explicit configuration

It is possible to provide a configuration file explicitly, via `--config` CLI parameter:

```
cardano-rt-view --config /path/to/your/configuration.yaml ...
```

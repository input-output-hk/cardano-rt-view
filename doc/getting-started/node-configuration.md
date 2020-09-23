# Cardano Node Configuration

## Prerequisites

It is assumed that you already have at least one instance of `cardano-node` which is already configured properly. If not - please read its [official documentation](https://docs.cardano.org/projects/cardano-node/en/latest/).

## RTView-related changes

Please open your configuration file for `cardano-node` and make the following changes. If you use default configuration files for `cardano-node`, you just have to uncomment a few lines.

### Setup backends

Add `TraceForwarderBK` in `setupBackends`, for example:

```
setupBackends:
  - KatipBK
  - TraceForwarderBK
```

### Map backends

Find the subsection `mapBackends` in the section `options` and add `TraceForwarderBK` to required tracers. For example, if you add it to `cardano.node.version` like this:

```
    cardano.node.version:
      - TraceForwarderBK
```

log message(s) sent to `cardano.node.version` will be sent to `TraceForwarderBK` and will be forwarded to RTView.

### Define forwarder endpoints

Find the section `traceForwardTo` and specify endpoints to connect with RTView. There are two possible ways to connect: pipes (default way for the local connection) and network sockets.

Example using UNIX pipes (on Linux and macOS):

```
traceForwardTo:
  tag: RemotePipe
  contents: "/tmp/rt-view-pipes/node-1"
```

All messages forwarded by this node will be sent to the pipe `node-1` created in `/tmp/rt-view-pipes`.

Example using Windows named pipes:

```
traceForwardTo:
  tag: RemotePipe
  contents: "\\\\.\\pipe\\acceptor"
```

The idea is the same as with UNIX pipes.

Example using network socket:

```
traceForwardTo:
  tag: RemoteSocket
  contents:
    - "127.0.0.1"
    - "2997"
```

All messages forwarded by this node will be sent to `http://127.0.0.1:2997` socket (please make sure you specify non-privileged port).

Please note that the settings in `traceForwardTo` section are necessary during RTView interactive dialog.

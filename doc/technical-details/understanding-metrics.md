# Understanding Metrics

Cardano node forwards its metrics and RTView accepts them. This guide describes some technical details behind it.

## Processes interaction

Interaction between processes can be shown schematically like this:

```
+=================================+    +================================+
| cardano-node                    |    | RTView                         |
| +------------------+            |    |            +-----------------+ |
| | iohk-monitoring  |            |    |            | iohk-monitoring | |
| |                  +----------+ |    | +----------+                 | |
| | TraceForwarderBK | Endpoint |------>>| Endpoint | TraceAcceptorBK | |
| +------------------+----------+ |    | +----------+-----------------+ |
+=================================+    |                 +------------+ |     +=============+
                                       |                 | Web-server |<<--->>| Web browser |
                                       |                 +------------+ |     +=============+
                                       +================================+
```

You can think of the library `iohk-monitoring` as an engine for forwarding/accepting metrics from the node to RTView. Mainly, there are two plugins in this library, `TraceForwarderBK` and `TraceAcceptorBK`, which are doing the job. That's why you should activate `TraceForwarderBK` in the node's configuration file.

More details about `iohk-monitoring` library can be found in [its repository](https://github.com/input-output-hk/iohk-monitoring-framework/).

## Tracers and metrics

Let's investigate where the metrics come from. For example, let's have a look at the node's version.

Both Pane mode and Grid node contain the field `Node version`, for example, `1.20.0`. This metric was traced by the node [here](https://github.com/input-output-hk/cardano-node/blob/4a8be018cc141023c688fd351e1eeea073f4bf18/cardano-node/src/Cardano/Node/Run.hs#L389), using `iohk-monitoring` library.

Since this metric was traced in `cardano.node.version` and the node's configuration contains these lines:

```
cardano.node.version:
  - TraceForwarderBK
```

this metric will be forwarded to RTView. After RTView receives this metric [here](https://github.com/input-output-hk/cardano-rt-view/blob/84081b64dfdfc57606d446190aabfe1f2928ab16/src/Cardano/RTView/NodeState/Updater.hs#L208-L211), it updates the web-page to display metric's value: `1.20.0`.

Please note that RTView updates the web-page not immediately, but every 2 seconds.

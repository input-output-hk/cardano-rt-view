# Understanding Metrics

Cardano node forwards its metrics, and RTView accepts these metrics. This guide describes some technical details behind it.

## Processes interaction

Schematically interaction between processes can be shown like this:

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

That's why `TraceForwarderBK` must be activated in node's configuration file: this `iohk-monitoring`'s plugin performs forwarding metrics to RTView.

## Tracers and metrics

Let's investigate where the metrics come from.

Both Pane mode and Grid node contain the field `Node version`, for example, `1.19.1`. This value was traced by the node [here](https://github.com/input-output-hk/cardano-node/blob/4a8be018cc141023c688fd351e1eeea073f4bf18/cardano-node/src/Cardano/Node/Run.hs#L389). That's why the node depends on `iohk-monitoring` library: it provides tracing functionality.

Since node's configuration contains these lines:

```
cardano.node.version:
  - TraceForwarderBK
```

this metric will be forwarded to RTView. After RTView receives this metric [here](https://github.com/input-output-hk/cardano-rt-view/blob/84081b64dfdfc57606d446190aabfe1f2928ab16/src/Cardano/RTView/NodeState/Updater.hs#L208-L211), it updates the web-page to display metric's value: `1.19.1`.

Please note that RTView updates the web-page not immediately, but every ~2 seconds.

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

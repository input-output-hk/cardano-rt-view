# Cardano Node Configuration

## Prerequisites

It is assumed that you already have at least one instance of `cardano-node`, which is already configured properly. If not - please read its [official documentation](https://docs.cardano.org/projects/cardano-node/en/latest/).

## RTView-related changes

After you finished the RTView configuration dialog, it displayed all the changes you have to make in your nodes' configuration files. For example, on Linux with all default values accepted, it looks like this:

```
1. Find setupBackends and add TraceForwarderBK in it:

   "setupBackends": [
     "TraceForwarderBK"
   ]

2. Find TurnOnLogMetrics and set it to True:

   "TurnOnLogMetrics": true

3. Find options -> mapBackends and redirect required metrics to TraceForwarderBK, for example:

   "options": {
     "mapBackends": {
       "cardano.node-metrics": [
         "TraceForwarderBK"
       ],
       "cardano.node.Forge.metrics": [
         "TraceForwarderBK"
       ],
       ...
     }

   For more info about supported metrics please read the documentation.

4. Since you have 3 nodes, add following traceForwardTo sections in the root of their configuration files:

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "0.0.0.0",
       "3000"
     ]
   }

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "0.0.0.0",
       "3001"
     ]
   }

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "0.0.0.0",
       "3002"
     ]
   }
```

## Cardano node configuration file: complete example

This is an example of the node's configuration file prepared for working with RTView on Linux:

```
{
  "ApplicationName": "cardano-sl",
  "ApplicationVersion": 1,
  "ByronGenesisFile": "mainnet-byron-genesis.json",
  "ByronGenesisHash": "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
  "LastKnownBlockVersion-Alt": 0,
  "LastKnownBlockVersion-Major": 2,
  "LastKnownBlockVersion-Minor": 0,
  "MaxKnownMajorProtocolVersion": 2,
  "Protocol": "Cardano",
  "RequiresNetworkMagic": "RequiresNoMagic",
  "ShelleyGenesisFile": "mainnet-shelley-genesis.json",
  "ShelleyGenesisHash": "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81",
  "TraceBlockFetchClient": false,
  "TraceBlockFetchDecisions": false,
  "TraceBlockFetchProtocol": false,
  "TraceBlockFetchProtocolSerialised": false,
  "TraceBlockFetchServer": false,
  "TraceChainDb": true,
  "TraceChainSyncBlockServer": false,
  "TraceChainSyncClient": false,
  "TraceChainSyncHeaderServer": false,
  "TraceChainSyncProtocol": false,
  "TraceDNSResolver": true,
  "TraceDNSSubscription": true,
  "TraceErrorPolicy": true,
  "TraceForge": true,
  "TraceHandshake": false,
  "TraceIpSubscription": true,
  "TraceLocalChainSyncProtocol": false,
  "TraceLocalErrorPolicy": true,
  "TraceLocalHandshake": false,
  "TraceLocalTxSubmissionProtocol": false,
  "TraceLocalTxSubmissionServer": false,
  "TraceMempool": true,
  "TraceMux": false,
  "TraceTxInbound": false,
  "TraceTxOutbound": false,
  "TraceTxSubmissionProtocol": false,
  "TracingVerbosity": "NormalVerbosity",
  "TurnOnLogMetrics": true,
  "TurnOnLogging": true,
  "ViewMode": "SimpleView",
  "defaultBackends": [
    "KatipBK"
  ],
  "defaultScribes": [
    [
      "StdoutSK",
      "stdout"
    ]
  ],
  "hasEKG": 12788,
  "hasPrometheus": [
    "127.0.0.1",
    12798
  ],
  "minSeverity": "Info",
  "options": {
    "mapBackends": {
      "cardano.node-metrics": [
        "EKGViewBK",
        "TraceForwarderBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.BlockFetchDecision.peers": [
        "EKGViewBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.ChainDB.metrics": [
        "EKGViewBK",
        "TraceForwarderBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.Forge.metrics": [
        "EKGViewBK",
        "TraceForwarderBK"
      ],
      "cardano.node.metrics": [
        "EKGViewBK",
        "TraceForwarderBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.metrics.peersFromNodeKernel": [
        "TraceForwarderBK"
      ]
    },
    "mapSubtrace": {
      "#ekgview": {
        "contents": [
          [
            {
              "contents": "cardano.epoch-validation.benchmark",
              "tag": "Contains"
            },
            [
              {
                "contents": ".monoclock.basic.",
                "tag": "Contains"
              }
            ]
          ],
          [
            {
              "contents": "cardano.epoch-validation.benchmark",
              "tag": "Contains"
            },
            [
              {
                "contents": "diff.RTS.cpuNs.timed.",
                "tag": "Contains"
              }
            ]
          ],
          [
            {
              "contents": "#ekgview.#aggregation.cardano.epoch-validation.benchmark",
              "tag": "StartsWith"
            },
            [
              {
                "contents": "diff.RTS.gcNum.timed.",
                "tag": "Contains"
              }
            ]
          ]
        ],
        "subtrace": "FilterTrace"
      },
      "benchmark": {
        "contents": [
          "GhcRtsStats",
          "MonotonicClock"
        ],
        "subtrace": "ObservableTrace"
      },
      "cardano.epoch-validation.utxo-stats": {
        "subtrace": "NoTrace"
      },
      "cardano.node-metrics": {
        "subtrace": "Neutral"
      },
      "cardano.node.metrics": {
        "subtrace": "Neutral"
      }
    }
  },
  "rotation": {
    "rpKeepFilesNum": 10,
    "rpLogLimitBytes": 5000000,
    "rpMaxAgeHours": 24
  },
  "setupBackends": [
    "KatipBK",
    "TraceForwarderBK"
  ],
  "setupScribes": [
    {
      "scFormat": "ScText",
      "scKind": "StdoutSK",
      "scName": "stdout",
      "scRotation": null
    }
  ],
  "traceForwardTo": {
    "tag": "RemoteSocket",
    "contents": [
      "0.0.0.0",
      "3000"
    ]
  }
}
```

This configuration file is based on [this Mainnet configuration](https://hydra.iohk.io/build/4553119/download/1/mainnet-config.json).

## Metrics routing

After you finished the RTView configuration, you saw a few changes that should be made in the node's configuration file. Particularly, the step `3` said:

```
3. Find options -> mapBackends and redirect required metrics to TraceForwarderBK, for example:

   "options": {
     "mapBackends": {
       "cardano.node-metrics": [
         "TraceForwarderBK"
       ],
       "cardano.node.Forge.metrics": [
         "TraceForwarderBK"
       ],
       ...
     }
```

These two lines, `cardano.node.metrics` and `cardano.node.Forge.metrics`, are tracers' names. You can think of tracers as points inside of Cardano node, and the node can send different values in these points. When you map the tracer `cardano.node.metrics` on the `TraceForwarderBK`, all the metrics from `cardano.node.metrics` will be sent to `TraceForwarderBK`. But if you remove `TraceForwarderBK` from `cardano.node.metrics`, all the metrics sent to this tracer will never be forwarded to RTView.

For more info about tracers, please read `Technical Details` -> `Understanding Metrics`.

These are supported tracers:

| Tracer                                     | Metric in Pane mode                    | Metric in Grid mode       |
| ------------------------------------------ | -------------------------------------- | ------------------------- |
| `cardano.node.metrics.peersFromNodeKernel` | `Peers` -> peers list                  | `Peers number`            |
| `cardano.node.metrics`                     | `Node info` -> `Node uptime`           | `Node uptime`             |
|                                            | `Mempool` -> `Mempool | TXs`           | `TXs in mempool, number`  |
|                                            | `Mempool` -> `Mempool | bytes`         | `Txs in mempool, bytes`   |
|                                            | `Mempool` -> `TXs processed`           | `TXs processed`           |
|                                            | `Blockchain` -> `Forged blocks number` | `Forged blocks number`    |
|                                            | `Blockchain` -> `Slot leader, number`  | `Slot leader, number`     |
|                                            | `Blockchain` -> `Cannot forge, number` | `Cannot forge, number`    |
|                                            | `Blockchain` -> `Missed slots number`  | `Missed slots number`     |
| `cardano.node-metrics`                     | `Resources` -> `Memory`                | `Memory usage`            |
|                                            | `Resources` -> `CPU`                   | `CPU usage`               |
|                                            | `Resources` -> `Disk`                  | `Disk usage`              |
|                                            | `Resources` -> `Network`               | `Network usage`           |
|                                            | `Node info` -> `Node platform`         | `Node platform`           |
|                                            | `RTS GC` -> `GC CPU time`              | `GC CPU time`             |
|                                            | `RTS GC` -> `GC time elapsed`          | `GC time elapsed`         |
|                                            | `RTS GC` -> `Number of GC runs`        | `Number of GC runs`       |
|                                            | `RTS GC` -> `Major GC runs`            | `Major GC runs`           |
| `cardano.node.ChainDB.metrics`             | `Blockchain` -> `Epoch`                | `Epoch`                   |
|                                            | `Blockchain` -> `Slot in epoch`        | `Slot in epoch`           |
|                                            | `Blockchain` -> `Blocks number`        | `Blocks number`           |
|                                            | `Blockchain` -> `Chain density`        | `Chain density`           |
| `cardano.node.Forge.metrics`               | `KES` -> `Start KES period`            | `Start KES period`        |
|                                            | `KES` -> `KES period`                  | `Current KES period`      |
|                                            | `KES` -> `KES remaining`               | `KES remaining periods`   |

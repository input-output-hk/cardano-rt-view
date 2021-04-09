# Cardano Node Configuration

## Prerequisites

It is assumed that you already have at least one instance of `cardano-node`, which is already configured properly. If not - please read its [official documentation](https://docs.cardano.org/projects/cardano-node/en/latest/).

## RTView-related changes

After you finished the RTView configuration dialog, it displayed all the changes you have to make in your nodes' configuration files. For example, on Linux with all default values accepted, it looks like this:

```
1. Find TurnOnLogMetrics flag and make sure it is true:

   "TurnOnLogMetrics": true

2. Since you have 3 nodes, add following traceForwardTo sections in the root of their configuration files:

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "<RTView_IPaddr>",
       "3000"
     ]
   }

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "<RTView_IPaddr>",
       "3001"
     ]
   }

   "traceForwardTo": {
     "tag": "RemoteSocket",
     "contents": [
       "<RTView_IPaddr>",
       "3002"
     ]
   }

  where <RTView_IPaddr> is the interface IP address where RTView is listening (can be 127.0.0.1 or internal network address i.e. 192.168.1.1 or public IP address)

3. extend `defaultbackends` with `TraceForwarderBK`

  "defaultBackends": [
    "KatipBK",
    "TraceForwarderBK"
  ]

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
    "KatipBK",
    "TraceForwarderBK"
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
      "cardano.node.metrics": [
        "EKGViewBK"
      ],
      "cardano.node.BlockFetchDecision.peers": [
        "EKGViewBK"
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
    "KatipBK"
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
      "127.0.0.1",
      "3000"
    ]
  }
}
```

This configuration file is based on [this Mainnet configuration](https://hydra.iohk.io/build/4553119/download/1/mainnet-config.json).

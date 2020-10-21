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
     "tag": "RemotePipe",
     "contents": "/run/user/1000/rt-view-pipes/node-1"
   }

   "traceForwardTo": {
     "tag": "RemotePipe",
     "contents": "/run/user/1000/rt-view-pipes/node-2"
   }

   "traceForwardTo": {
     "tag": "RemotePipe",
     "contents": "/run/user/1000/rt-view-pipes/node-3"
   }
```

## Cardano node configuration file: YAML example

This is an example of the node's configuration file prepared for working with RTView on Linux:

```
##########################################################
###############          Mainnet                 #########
############### Cardano Byron Node Configuration #########
##########################################################


##### Locations #####

GenesisFile: genesis/genesis.json
SocketPath: db/node.socket

##### Blockfetch Protocol

# The maximum number of used peers during bulk sync.
MaxConcurrencyBulkSync: 1
# The maximum number of used peers when fetching newly forged blocks.
MaxConcurrencyDeadline: 2

#TODO: These parameters cannot yet be used in the config file, only on the CLI:
#DatabasePath: db/
#Topology: configuration/mainnet-topology.json
#Port 7776

##### Core protocol parameters #####

# This is the instance of the Ouroboros family that we are running.
Protocol: Shelley

# The mainnet does not include the network magic into addresses. Testnets do.
RequiresNetworkMagic: RequiresMagic


##### Update system parameters #####

# This protocol version number gets used by by block producing nodes as part
# part of the system for agreeing on and synchronising protocol updates.
LastKnownBlockVersion-Major: 0
LastKnownBlockVersion-Minor: 2
LastKnownBlockVersion-Alt: 0

# In the Byron era some software versions are also published on the chain.
# We do this only for Byron compatibility now.
ApplicationName: cardano-sl
ApplicationVersion: 1


##### Logging configuration #####

# The node can run in either the SimpleView or LiveView. The SimpleView just
# uses standard output, optionally with log output. The LiveView is a text
# console on Linux and Mac OSX with a live view of various node metrics.
# When LiveView is used logging output to 'stdout' is automatically disabled.
ViewMode: SimpleView
#ViewMode: LiveView

# Enable or disable logging overall
TurnOnLogging: True

# Enable the collection of various OS metrics such as memory and CPU use.
# These metrics are traced in the context name: 'cardano.node-metrics' and can
# be directed to the logs or monitoring backends.
TurnOnLogMetrics: True

# Global logging severity filter. Messages must have at least this severity to
# pass. Typical values would be Warning, Notice, Info or Debug.
minSeverity: Debug

# Log items can be rendered with more or less verbose detail.
# Verbosity ranges from MinimalVerbosity, NormalVerbosity to MaximalVerbosity
TracingVerbosity: NormalVerbosity

# The system supports a number of backends for logging and monitoring.
# This setting lists the backends that will be available to use in the
# configuration below. The logging backend is called Katip.
setupBackends:
  - KatipBK
  - TraceForwarderBK

# This specifies the default backends that trace output is sent to if it
# is not specifically configured to be sent to other backends.
defaultBackends:
  - KatipBK

# EKG is a simple metrics monitoring system. Uncomment the following to enable
# this backend and listen on the given local port and point your web browser to
# http://localhost:12788/
# hasEKG: 12788

# The Prometheus monitoring system exports EKG metrics. Uncomment the following
# to listen on the given port. Output is provided on
# http://localhost:12789/metrics
# hasPrometheus:
#   - "127.0.0.1"
#   - 12789

# For the Katip logging backend we must set up outputs (called scribes)
# The available types of scribe are:
#   FileSK for files
#   StdoutSK/StderrSK for stdout/stderr
#   JournalSK for systemd's journal system
#   DevNullSK ignores all output
# The scribe output format can be ScText or ScJson. Log rotation settings can
# be specified in the defaults below or overidden on a per-scribe basis here.
setupScribes:
  - scKind: FileSK
    scName: "logs/mainnet.log"
    scFormat: ScText

  - scKind: StdoutSK
    scName: stdout
    scFormat: ScText

# For the Katip logging backend this specifies the default scribes that trace
# output is sent to if it is not configured to be sent to other scribes.
defaultScribes:
  - - FileSK
    - "logs/mainnet.log"
  - - StdoutSK
    - stdout

# The default file rotation settings for katip scribes, unless overridden
# in the setupScribes above for specific scribes.
rotation:
  rpLogLimitBytes: 5000000
  rpKeepFilesNum:  3
  rpMaxAgeHours:   24

##### Coarse grained logging control #####

# Trace output from whole subsystems can be enabled/disabled using the following
# settings. This provides fairly coarse grained control, but it is relatively
# efficient at filtering out unwanted trace output.

# Trace BlockchainTime.
TraceBlockchainTime: True

# Trace BlockFetch client.
TraceBlockFetchClient: True

# Trace BlockFetch decisions made by the BlockFetch client.
# needed to display "peers" and their block height in LiveView
TraceBlockFetchDecisions: True

# Trace BlockFetch protocol messages.
TraceBlockFetchProtocol: True

# Serialised Trace BlockFetch protocol messages.
TraceBlockFetchProtocolSerialised: True

# Trace BlockFetch server.
TraceBlockFetchServer: True

# Verbose tracer of ChainDB
TraceChainDb: True

# Trace ChainSync client.
TraceChainSyncClient: True

# Trace ChainSync server (blocks).
TraceChainSyncBlockServer: True

# Trace ChainSync server (headers)
TraceChainSyncHeaderServer: True

# Trace ChainSync protocol messages.
TraceChainSyncProtocol: True

# Trace DNS Resolver messages.
TraceDNSResolver: True

# Trace DNS Subscription messages.
TraceDNSSubscription: True

# Trace error policy resolution.
TraceErrorPolicy: True

# Trace local error policy resolution.
TraceLocalErrorPolicy: True

# Trace block forging.
TraceForge: True

# Trace Handshake protocol messages.
TraceHandshake: True

# Trace IP Subscription messages.
TraceIpSubscription: True

# Trace local ChainSync protocol messages.
TraceLocalChainSyncProtocol: True

# Trace local Handshake protocol messages.
TraceLocalHandshake: True

# Trace local TxSubmission protocol messages.
TraceLocalTxSubmissionProtocol: True

# Trace local TxSubmission server.
TraceLocalTxSubmissionServer: True

# Trace mempool.
TraceMempool: True

# Trace Mux Events
TraceMux: True

# Trace TxSubmission server (inbound transactions).
TraceTxInbound: True

# Trace TxSubmission client (outbound transactions).
TraceTxOutbound: True

# Trace TxSubmission protocol messages.
TraceTxSubmissionProtocol: True

##### Fine grained logging control #####

# It is also possible to have more fine grained control over filtering of
# trace output, and to match and route trace output to particular backends.
# This is less efficient than the coarse trace filters above but provides
# much more precise control.

options:

  # This routes metrics matching specific names to particular backends.
  # This overrides the 'defaultBackends' listed above. And note that it is
  # an override and not an extension so anything matched here will not
  # go to the default backend, only to the explicitly listed backends.
  mapBackends:
    cardano.node.BlockFetchDecision.peers:
      - TraceForwarderBK
      - EKGViewBK
      - kind: UserDefinedBK
        name: LiveViewBackend
    cardano.node.ChainDB.metrics:
      - TraceForwarderBK
      - EKGViewBK
      - kind: UserDefinedBK
        name: LiveViewBackend
    cardano.node-metrics:
      - TraceForwarderBK
    cardano.node.metrics:
      - TraceForwarderBK
      - EKGViewBK
      - kind: UserDefinedBK
        name: LiveViewBackend
    cardano.node.Forge.metrics:
      - TraceForwarderBK
      - EKGViewBK
    cardano.node.release:
      - TraceForwarderBK
      - KatipBK
    cardano.node.version:
      - TraceForwarderBK
      - KatipBK
    cardano.node.commit:
      - TraceForwarderBK
      - KatipBK

  # redirects traced values to a specific scribe which is identified by its
  # type and its name, separated by "::":
  #mapScribes:
  #  cardano.node-metrics:
  #    - "FileSK::logs/mainnet.log"

  # apply a filter on message severity on messages in a specific named context.
  # this filter is applied additionally to the global 'minSeverity' and thus
  # needs to be at least as high.
  mapSeverity:
    cardano.node.ChainDB: Notice
    cardano.node.DnsSubscription: Debug

traceForwardTo:
  tag: RemotePipe
  contents: "/run/user/1000/rt-view-pipes/node-1"
```

This configuration file is based on [lite/configuration/shelley-1.yaml](https://github.com/input-output-hk/cardano-node/blob/6229d7c6aef8af0f36abea30227b864453be3b5e/scripts/lite/configuration/shelley-1.yaml) from `cardano-node` repository.

## Cardano node configuration file: JSON example

This is the same configuration file in JSON format:

```
{
  "GenesisFile": "genesis/genesis.json",
  "SocketPath": "db/node.socket",
  "MaxConcurrencyBulkSync": 1,
  "MaxConcurrencyDeadline": 2,
  "Protocol": "Shelley",
  "RequiresNetworkMagic": "RequiresMagic",
  "LastKnownBlockVersion-Major": 0,
  "LastKnownBlockVersion-Minor": 2,
  "LastKnownBlockVersion-Alt": 0,
  "ApplicationName": "cardano-sl",
  "ApplicationVersion": 1,
  "ViewMode": "SimpleView",
  "TurnOnLogging": true,
  "TurnOnLogMetrics": true,
  "minSeverity": "Debug",
  "TracingVerbosity": "NormalVerbosity",
  "setupBackends": [
    "KatipBK",
    "TraceForwarderBK"
  ],
  "defaultBackends": [
    "KatipBK"
  ],
  "setupScribes": [
    {
      "scKind": "FileSK",
      "scName": "logs/mainnet.log",
      "scFormat": "ScText"
    },
    {
      "scKind": "StdoutSK",
      "scName": "stdout",
      "scFormat": "ScText"
    }
  ],
  "defaultScribes": [
    [
      "FileSK",
      "logs/mainnet.log"
    ],
    [
      "StdoutSK",
      "stdout"
    ]
  ],
  "rotation": {
    "rpLogLimitBytes": 5000000,
    "rpKeepFilesNum": 3,
    "rpMaxAgeHours": 24
  },
  "TraceBlockchainTime": true,
  "TraceBlockFetchClient": true,
  "TraceBlockFetchDecisions": true,
  "TraceBlockFetchProtocol": true,
  "TraceBlockFetchProtocolSerialised": true,
  "TraceBlockFetchServer": true,
  "TraceChainDb": true,
  "TraceChainSyncClient": true,
  "TraceChainSyncBlockServer": true,
  "TraceChainSyncHeaderServer": true,
  "TraceChainSyncProtocol": true,
  "TraceDNSResolver": true,
  "TraceDNSSubscription": true,
  "TraceErrorPolicy": true,
  "TraceLocalErrorPolicy": true,
  "TraceForge": true,
  "TraceHandshake": true,
  "TraceIpSubscription": true,
  "TraceLocalChainSyncProtocol": true,
  "TraceLocalHandshake": true,
  "TraceLocalTxSubmissionProtocol": true,
  "TraceLocalTxSubmissionServer": true,
  "TraceMempool": true,
  "TraceMux": true,
  "TraceTxInbound": true,
  "TraceTxOutbound": true,
  "TraceTxSubmissionProtocol": true,
  "options": {
    "mapBackends": {
      "cardano.node.BlockFetchDecision.peers": [
        "TraceForwarderBK",
        "EKGViewBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.ChainDB.metrics": [
        "TraceForwarderBK",
        "EKGViewBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node-metrics": [
        "TraceForwarderBK"
      ],
      "cardano.node.metrics": [
        "TraceForwarderBK",
        "EKGViewBK",
        {
          "kind": "UserDefinedBK",
          "name": "LiveViewBackend"
        }
      ],
      "cardano.node.Forge.metrics": [
        "TraceForwarderBK",
        "EKGViewBK"
      ],
      "cardano.node.release": [
        "TraceForwarderBK",
        "KatipBK"
      ],
      "cardano.node.version": [
        "TraceForwarderBK",
        "KatipBK"
      ],
      "cardano.node.commit": [
        "TraceForwarderBK",
        "KatipBK"
      ]
    },
    "mapSeverity": {
      "cardano.node.ChainDB": "Notice",
      "cardano.node.DnsSubscription": "Debug"
    }
  },
  "traceForwardTo": {
    "tag": "RemotePipe",
    "contents": "/run/user/1000/rt-view-pipes/node-1"
  }
}
```

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
| `cardano.node.release`                     | `Node info` -> `Node protocol`         | `Node protocol`           |
| `cardano.node.version`                     | `Node info` -> `Node version`          | `Node version`            |
| `cardano.node.commit`                      | `Node info` -> `Node commit`           | `Node commit`             |


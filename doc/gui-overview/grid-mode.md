# GUI Overview: Grid mode

![Grid mode](../images/screenshot-grid-mode.png)

The page in grid mode displays the table where columns correspond to the nodes, and rows correspond to the metrics.

## Select node

At the top bar of the page, you will find the dropdown list `Select node`. Here you can select the nodes you want to see:

1. Check the checkbox if you want to see the corresponding panel;
2. Uncheck it if you're going to hide it.

## Select metric

At the top bar of the page, you will find the second dropdown list `Select metric`. Here you can select the metric you want to see:

1. Check the checkbox if you want to see the corresponding metric;
2. Uncheck it if you're going to hide it.

## Metrics

* `TraceAcceptor endpoint` - network socket or the pipe used to connect this node with RTView;
* `Node protocol` - node's protocol (for example, `Shelley`);
* `Node version` - version of the node;
* `Node platform` - a platform the node is working on (for example, `Linux`);
* `Node commit` - git commit the node was built from;
* `Peers number` - the number of connected peers;
* `Node uptime` - how long the node is working;
* `Start KES period` - certificate KES (Key Evolving Signature) start period;
* `Current KES period` - current KES period;
* `KES remaining periods` - KES periods until expiry;
* `Memory usage` - how much memory is consumed by the node, in MB;
* `CPU usage` - how many CPU is used by the node, in percents;
* `Disk usage` - node's disk activity, **read/write**-operations, in KB/s;
* `Network usage` - node's network activity, **input/output**-operations, in KB/s;
* `Epoch` - the number of current epoch;
* `Slot in epoch` - the number of the current slot;
* `Blocks number` - the number of blocks in this blockchain;
* `Forged blocks number` - the number of blocks forged by this node;
* `Cannot forge, number` - the number of slots when this node was a leader but because of misconfiguration (for example, invalid key), it's impossible to forge a new block;
* `Chain density` - chain density, in percents;
* `Slot leader, number` - the number of slots when this node was a leader;
* `Missed slots number` - the number of slots when this node was a leader but didn't forge a new block;
* `TXs processed` - the number of processed transactions in this blockchain. These transactions are already removed from the mempool so that we can treat them as completely processed;
* `TXs in mempool, number` - the number of transactions in the mempool;
* `Txs in mempool, bytes` - the number of transactions in the mempool, in bytes;
* `GC CPU time` - total CPU time used by the GC, in seconds;
* `GC time elapsed` - total elapsed time used by the GC, in seconds;
* `Number of GC runs` - total number of GCs;
* `Major GC runs` - total number of major (oldest generation) GCs.

For more information about KES, please read ["Key Evolving Signature and KES period" article](https://docs.cardano.org/projects/cardano-node/en/latest/stake-pool-operations/KES_period.html).

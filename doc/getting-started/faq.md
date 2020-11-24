# RTView: FAQ

## «I don't want to wait the next release - where can I find the latest builds of RTView?»

You can download the latest build from [Hydra CI](https://hydra.iohk.io/jobset/Cardano/cardano-rt-view).

## «What should I use, network sockets, or named pipes?»

Network sockets use the host and the port to connect RTView with your nodes. It looks like `123.45.67.89:3000`, where `123.45.67.89` is a public IP address and `3000` is an opened port.

Named pipes use special files to connect RTView with your nodes. It looks like `/run/user/1000/rt-view-pipes/node-1` on Linux or `\\.\pipe\Users-Name-AppData-Local-Temp-_rt-view-pipes_node-1` on Windows.

Use named pipes if your nodes and RTView are running on the **same** machine, and you don't want to open a port on your localhost. If your nodes and RTView are running on **different** machines, or you can open a port on your localhost - use network sockets.

## «If I want to run RTView on one machine and the node on other one - can I use the different OS on these machines?»

Yes, you can. For example, your node can be launched on Linux machine, and RTView can be launched on Windows machine.

## «Where can I find an example of the node's configuration file for working with RTView?»

You can find it [here](https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/node-configuration.md#cardano-node-configuration-file-complete-example).

## «How many nodes can I connect to my RTView process?»

You can connect as many nodes as you want.

## «I know that particular error occurred, but I don't see it in the Errors tab. Why?»

Please make sure you added corresponding tracer name in your node's configuration file. You can find the details [here](https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/node-configuration.md#errors-routing).

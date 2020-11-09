# Install RTView

## Prerequisites

You will need:

1. An x86 host (AMD or Intel) with at least 2 cores, 4GB of RAM, and at least 100MB of free disk space;
2. A recent version of Linux, Windows or macOS;
3. Any modern web browser.

## Download and unpack

Please go to [releases page](https://github.com/input-output-hk/cardano-rt-view/releases) and download an archive for your platform, their names look like this:

1. `cardano-rt-view-*-darwin.zip`
2. `cardano-rt-view-*-linux-x86_64.tar.gz`
3. `cardano-rt-view-*-win64.zip`

Then unpack an archive, inside you will find an executable `cardano-rt-view`.

## macOS notes

To prevent a system warning about "an application downloaded from the Internet", run this command:

```
xattr -r -d com.apple.quarantine cardano-rt-view-*-darwin/*
```

## Run and configuration dialog

After you run an executable `cardano-rt-view`, an interactive dialog will be started:

```
RTView: real-time watching for Cardano nodes

Let's configure RTView...
```

The first question is:

```
How many nodes will you connect (1 - 99, default is 3):
```

Please input the number of `cardano-node` processes that should forward their metrics to RTView. Press the "Enter" key if you want to use the default value.

The next question is:

```
Input the names of the nodes (default are "node-1", "node-2", "node-3"), one at a time:
```

From RTView's point of view, each `cardano-node` process that forwards its metrics to RTView, should be identified by a unique name. You can use any name you want (please note that name should not include spaces).

The next question is:

```
Indicate the port for the web server (1024 - 65535, default is 8024):
```

Please input the port RTView will use to display the web-page. For example, if you keep the default port `8024`, the web-page will be available on `http://127.0.0.1:8024`.

The next question is:

```
Indicate how your nodes should be connected with RTView: networking sockets <S> or named pipes <P>."
Default way is sockets, so if you are not sure - choose <S>:
```

Please choose the way how the nodes should be connected to RTView.

If you chose `S`, you will be asked about the base port:

```
Ok, sockets will be used. Indicate the base port to listen for connections (1024 - 65535, default is 3000):
```

The base port will be used for the first node that forwards its metrics to RTView. For example, if you will launch three `cardano-node` processes (`node-1`, `node-2`, and `node-3`) that will forward their metrics using network sockets, this is how they will be connected to RTView:

1. `node-1` -> `0.0.0.0:3000`
1. `node-2` -> `0.0.0.0:3001`
1. `node-3` -> `0.0.0.0:3002`

Bit if you selected `P`, you will be asked about the directory when pipes will be created:

```
Ok, pipes will be used. Indicate the directory for them, default is "/run/user/1000/rt-view-pipes":
```

The next question is:

```
Now, indicate a host of machine RTView will be launched on (default is 0.0.0.0):
```

If your nodes are launched on the same machine with RTView, you can choose the default address. But if RTView will be launched on another machine, please specify its reachable public IP address. It will allow your nodes to connect with RTView. In this case, it is assumed that a machine with RTView is accessible using that IP address.

The last question is:

```
Indicate the directory with static content for the web server, default is "static":
```

Since RTView displays nodes' metrics on the web-page, it uses static web content (CSS, JS, images). By default, it's `static` directory that is included in the archive you've downloaded.

Then you will see this message:

```
Great, RTView is ready to run! Its configuration was saved at PATH_TO/rt-view.yaml. Press <Enter> to continue...
```

where `PATH_TO` is the full path to the default system configuration directory.

After you pressed `Enter`, RTView will show all the changes you have to make in your node(s) configuration file(s). For example, if you chose all default values on Linux, you will see this:

```
Now you have to make the following changes in your node's configuration file:

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

After you are done, press <Enter> to run RTView...
```

After you pressed `Enter`, RTView will be launched, and you can open `http://127.0.0.1:8024` (if you chose default web-port) and see the web-page.

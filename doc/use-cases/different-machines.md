# Use Case: Different Machines

One of the main benefits of RTView is the ability of distributed work: you can run it on one machine and run your nodes on other ones. In this case, your nodes will send their metrics to RTView over the network. This guide describes how to set it up.

## Scenario

You have two machines:

1. machine for the RTView (let's call it `Server machine`),
2. machine for the node (let's call it `Client machine`).

So, one instance of the RTView will be launched on the `Server machine`, and one instance of the node will be launched on the `Client machine`. It is assumed that you have a network connection between `Server machine` and `Client machine`.

Additionally, it is assumed that you have some web-server (for example, NGINX) on the `Server machine`. It will allow you to access RTView's web-page from any device. But technically, web-server is not a requirement, because RTView anyway runs its web-server.

## Server machine setup

First of all, check the public IP of the `Server machine`:

```
curl ifconfig.me
```

Let's assume that it's `12.13.14.15`. Please remember this IP, you will need it soon.

Now, download RTView archive for your platform. For example, if `Server machine` works under Linux, download the latest `*.tar.gz` archive from [Releases page](https://github.com/input-output-hk/cardano-rt-view/releases). Unpack it and run:

```
tar -xvf cardano-rt-view-*-linux-x86_64.tar.gz
./cardano-rt-view
```

Example of answers during configuration dialog:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 RTView: real-time watching for Cardano nodes 
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Let's configure RTView...

How many nodes will you connect (1 - 99, default is 3): 1
Ok, 1 nodes.

Input the name of the node (default is "node-1"): 
Ok, default name "node-1" will be used.

Indicate the port for the web server (1024 - 65535, default is 8024): 
Ok, the web-page will be available on http://127.0.0.1:8024, on the machine RTView will be launched on.

Indicate how your nodes should be connected with RTView (pipes <P> or networking sockets <S>): s
Ok, sockets will be used. Indicate the port base to listen for connections (1024 - 65535, default is 3000): 
Ok, these ports will be used to accept nodes' metrics: 3000
Now, indicate a host of machine RTView will be launched on (default is 0.0.0.0): 12.13.14.15
Ok, it is assumed that RTView will be launched on the host "12.13.14.15".

Indicate the directory with static content for the web server, default is "static": 
Ok, default directory will be used.

Great, RTView is ready to run! Its configuration was saved at /home/denis/.config/rt-view.yaml. Press <Enter> to continue...
```

Please note that the public IP of the `Server machine` was indicated for the question `indicate a host of machine RTView will be launched on`. Now, after you press `Enter`, you will see the changes you have to make in your node's configuration file:

```
1. Find setupBackends and add TraceForwarderBK in it:

   setupBackends:
     - TraceForwarderBK

2. Find TurnOnLogMetrics and set it to True:

   TurnOnLogMetrics: True

3. Find options -> mapBackends and redirect required metrics to TraceForwarderBK, for example:

   options:
     mapBackends:
       cardano.node.metrics:
         - TraceForwarderBK
       cardano.node.Forge.metrics:
         - TraceForwarderBK

   For more info about supported metrics please read the documentation.

4. Since you have 1 node, add following traceForwardTo section in the root of its configuration file:

   traceForwardTo:
     tag: RemoteSocket
     contents:
       - "12.13.14.15"
       - "3000"
```

As you can see, the section `traceForwardTo` contains the `Server machine`'s public IP.

**IMPORTANT:** please make sure that corresponding port on the `Server machine` is available and open. In this example, the port `3000` should be open for the `Client machine`.

## Client machine setup

It is assumed that you already have the node on the `Client machine`. If not - please read [how to build it](https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/building-the-node-using-nix.html).

Now open your node's configuration file and make the changes you saw in the end of the RTView configuration dialog.

## Launch

Now go to the `Server machine` and launch RTView, then go to the `Client machine` and launch your node. After that, your node will connect to the RTView via `12.13.14.15:3000`. To see the RTView's web-page, please open `http://12.13.14.15:8024` (if you chose default port) in any browser.

## FAQ

### Can `Server machine` and the `Client machine` work on different platforms?

Yes. For example, your `Client machine` can use Linux, and you `Server machine` can use macOS.

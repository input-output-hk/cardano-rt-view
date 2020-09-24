# Install RTView

## Prerequisites

You will need:

1. An x86 host (AMD or Intel) with at least 2 cores, 4GB of RAM, and at least 100MB of free disk space;
2. A recent version of Linux, Windows or macOS;
3. Any modern web browser.

## Download and unpack

Please go to [releases page](https://github.com/input-output-hk/cardano-rt-view/releases) and download the package for your platform:

1. `cardano-rt-view-*-darwin.zip`
2. `cardano-rt-view-*-linux-x86_64.tar.gz`
3. `cardano-rt-view-*-win64.zip`

Then unpack an archive that contains an executable `cardano-rt-view`.

## Prepare to run

### macOS notes

To prevent a system warning about "an application downloaded from the Internet", run this command:

```
xattr -r -d com.apple.quarantine cardano-rt-view-*-darwin/*
```

### Windows notes

During the start, RTView initiates an interactive dialog that depends on the UTF-8 encoding. So, to prevent a system error:

```
<stdout>: commitAndReleaseBuffer: invalid argument (invalid character)
```

please go to `Settings` -> `Language Settings` -> `Administrative language settings`, then `Change system locale` and select `Beta: Use Unicode UTF-8 for worldwide language support`. Restart Windows after that.

## Run and configuration dialog

After you run an executable `cardano-rt-view`, an interactive dialog will be started.

The first question is:

```
How many nodes will you connect (1 - 99, default is 3):
```

Please input the number of `cardano-node` processes that should forward their metrics to RTView. Press the "Enter" key if you want to use the default value.

The next question is:

```
"Input the names of the nodes (default are "node-1", "node-2", "node-3"):
```

From RTView's point of view, each external process that forwards its metrics to RTView, should be identified by a unique name. You can use any name you want (please note that name does not include spaces).

The next question is:

```
Indicate the port for the web service (1024 - 65535, default is 8024):
```

Please input the port RTView will use to display the web-page. For example, if you keep the default port `8024`, RTView will start the web server that listens on `http://127.0.0.1:8024`.

The next question is:

```
Connections shall be made via pipes (P, default way) or networking sockets (S)?
```

Please choose the way how the nodes should be connected to RTView. If you selected `P`, you will be asked about the directory when pipes will be created:

```
Ok, pipes will be used. Indicate the directory for them, default is "/tmp/rt-view-pipes":
```

But if you chose `S`, you will be asked about the base port:

```
Ok, sockets will be used. Indicate the port base to listen for connections (1024 - 65535, default is 3000):
```

The base port will be used for the first node that forwards its metrics to RTView. For example, if you will launch three `cardano-node` processes that will forward their metrics to the network sockets, this is how they will be connected to RTView:

1. first node -> `http://127.0.0.1:3000`
1. second node -> `http://127.0.0.1:3001`
1. third node -> `http://127.0.0.1:3002`

**Important**: Please make sure your connection settings correspond to the section `traceForwardTo` in your node(s) configuration file(s).

The last question is:

```
"Indicate the directory with static content for the web service, default is "static":
```

Since RTView displays nodes' metrics on the web-page, it uses static web content (CSS styles, JS, images). By default, it's `static` directory that is included in the package you've downloaded.

After that, RTView will be launched, and you can open `http://127.0.0.1:8024` (if you chose default web-port) and see the web-page.

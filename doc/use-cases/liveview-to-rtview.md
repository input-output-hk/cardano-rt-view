# Use Case: From LiveView to RTView

Many RTView users were LiveView users. But since LiveView was deprecated and removed from Cardano node, this guide describes how to move from LiveView to RTView.

## Main difference

The main difference between LiveView and RTView is this:

* LiveView was a part of `cardano-node` executable.
* RTView isn't a part of `cardano-node` executable; it is a separate application.

That's why LiveView was available immediately: you turn `ViewMode` to `LiveView` in your node's configuration file, and it's already here, right in your Unix terminal.

But RTView is a separate application: you should download, configure, and launch it separately from your node.

## Understand concepts

1. LiveView is a part of the node, so if you have ten nodes, you need ten terminal windows to see their metrics. RTView receives metrics from your nodes, so if you have ten nodes, you need only one RTView to show their metrics.
2. LiveView provides TUI (textual UI) in the terminal. RTView provides web UI in any browser.
3. LiveView's TUI is Unix-based software, so you cannot run the node on Windows in LiveView mode. RTView is a cross-platform application so that you can run it on Windows, Linux, or macOS.

Because of web UI, you can run RTView on a headless machine with a web-server and use the browser on any connected device (laptop, tablet, or phone) to see the metrics from your nodes. Please see [use case example](https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/use-cases/different-machines.md#use-case-different-machines).

## RTView workflow

1. Install and configure RTView. Please read [this guide](https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/install.md#install-rtview).
2. Configure your nodes. Please read [this guide](https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/node-configuration.md#cardano-node-configuration).
3. Run RTView.
4. Run your nodes.

After that, open the web-page and see the metrics received from your nodes.

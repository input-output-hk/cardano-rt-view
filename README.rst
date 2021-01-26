.. raw:: html

   <p align="center">
     <a href="https://github.com/input-output-hk/cardano-rt-view/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-rt-view.svg?style=for-the-badge" /></a>
     &nbsp;
     <a href="https://hydra.iohk.io/jobset/Cardano/cardano-rt-view"><img src="https://img.shields.io/badge/Hydra-CI-brightgreen?style=for-the-badge" /></a>
     &nbsp;
     <a href="https://github.com/input-output-hk/cardano-rt-view/issues"><img src="https://img.shields.io/github/issues/input-output-hk/cardano-rt-view?style=for-the-badge"></a>
   </p>

********************************************
RTView: real-time watching for Cardano nodes
********************************************

What is it
==========

RTView is an application that allows seeing the state of running `Cardano nodes <https://github.com/input-output-hk/cardano-node/>`_ in real-time.

Who is this for
===============

RTView is useful for anyone who runs Cardano nodes and wants to see what is going on. You can view the information about the nodes, peers, blockchain, transactions, resources, etc.

Key features
============

1. **Multiple nodes**: connect as many nodes as you want, whether they run locally or on different machines.
2. **Web-based UI**: it provides configurable sections, live charts, and interactive tooltips. Use any browser or run RTView behind a web server like NGINX and view your nodes' state on any third-party device.
3. **Email notifications**: get notified about any problems with your nodes.

How it looks like
=================

The web-page with 3 nodes may look like this:

.. image:: https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/images/screenshot.png
  :width: 500
  :alt: Screenshot

Download
========

Stable release packages for Linux and macOS are available `here <https://github.com/input-output-hk/cardano-rt-view/releases>`_.

Also, you can download the latest builds from Hydra CI:

1. `Linux <https://hydra.iohk.io/job/Cardano/cardano-rt-view/cardano-rt-view-linux-release/latest/download/1>`_
2. `macOS <https://hydra.iohk.io/job/Cardano/cardano-rt-view/cardano-rt-view-darwin-release/latest/download/1>`_

Documentation
=============

Recommended topics:

* `Overview <https://docs.cardano.org/en/latest/rt-view/rt-view.html>`_
* `Quick start <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/install.md>`_
* `Configure your nodes <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/node-configuration.md>`_
* `GUI overview <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/gui-overview/overview.md>`_
* `FAQ <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/faq.md>`_

Additional topics:

* `Build RTView from sources <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/building-rt-view-from-sources.md>`_
* `Learn more about RTView configuration <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/getting-started/rt-view-configuration.md>`_
* `Understanding node's metrics <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/technical-details/understanding-metrics.md>`_
* `Email notifications <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/technical-details/email-notifications.md>`_

Use cases:

* `Distributed mode: step by step <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/use-cases/different-machines.md>`_
* `LiveView to RTView <https://github.com/input-output-hk/cardano-rt-view/blob/master/doc/use-cases/liveview-to-rtview.md>`_

.. raw:: html

   <hr/>

   <p align="center">
     <a href="https://github.com/input-output-hk/cardano-rt-view/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-rt-view.svg?style=for-the-badge" /></a>
   </p>

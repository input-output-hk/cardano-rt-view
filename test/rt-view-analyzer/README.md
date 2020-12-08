# RTView: Automatic Testing

## How It Works

RTView is a separate process that receives `LogObject`s from another process (for example, `cardano-node`) and displays them on HTML-page. It can be shown like this:

```
Process 1                        Process 2                             Process 3
+--------------+   [LogObject]   +-----------------+    web-requests   +-------------+
| cardano-node |  ------------>  | cardano-rt-view |  <--------------  | web browser |
+--------------+                 +-----------------+  -------------->  +-------------+
                                                         HTML-page
```

To test it automatically, we use additional scripts and programs:

1. `sender.sh` - it takes `LogObject`s from predefined JSON-file and sends them to RTView (via UNIX-socket).
2. `analyzer` - it analyzes RTView UI (complete HTML-page).

It can be shown like this:

```
                                                  Process 1                             Process 2
                    +-----------+   [LogObject]   +-----------------+    web-requests   +----------+
logObjects.json --> | sender.sh |  ------------>  | cardano-rt-view |  <--------------  | analyzer |
                    +-----------+                 +-----------------+  -------------->  +----------+
                                                                          HTML-page
```

Please note that `analyzer` doesn't analyze RTView UI (complete HTML-page) by itself. Instead, it launches the real web browser and use it to analyze the page automatically, using Selenium standalone server and `webdriver` package. It can be shown like this:

```
+----------+       +-------------+       +---------+  web-commands  +-----------------+
| analyzer | ----> | GeckoDriver | ----> | Firefox | -------------> | cardano-rt-view |
+----------+       +-------------+       +---------+                +-----------------+
       \
        \          +-----------------+
         `-------> | Selenium server |
                   +-----------------+
```

## Required Software

Please make sure you have these commands in your `PATH`:

1. `jq` to minimize predefined JSON-file with `LogObject`s.
2. `nc` to send `LogObject`s from predefined JSON-file to `cardano-rt-view` (via UNIX socket).
3. `firefix` to interact with RTView UI (complete HTML-page),
4. `geckodriver` to interact with Firefox,
5. `java` to launch `selenium-server-standalone`.

Please note that you have to provide full path to `selenium-server-standalone` file (something like `selenium-server-standalone-3.141.59.jar`) to `runTest.sh` script (see below). `selenium-server-standalone` will be used by `webdriver` package. This server can be downloaded [here](https://www.selenium.dev/downloads/).

## How to Run It

Run `./runTest.sh <options> PATH_TO_SELENIUM_SERVER_JAR` script which launches:

1. `cardano-rt-view` process (in the background),
2. `sender.sh` script,
3. `selenium-server-standalone` process (in the background),
4. `analyzer` process.

(<options> can be something like `--nix` to select the builder)

`analyzer` process launches Firefox web browser and sends corresponding web-commands to it. The results returned by `analyzer` are the results of the test.

## What Do We Test?

`analyzer` checks the visual behavior of RTView UI (complete HTML-page):

1. checks if node can be hidden/shown,
2. validates the real values of displayed metrics (for comparing them with values from predefined JSON-file with `LogObject`s).

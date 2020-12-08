# RTView Configuration

## Saved configuration

During the start, RTView initiates an interactive dialog with the user. Based on the given answers, it creates a configuration file (see an example below). This file will be automatically saved on your hard drive in the system default configuration directory, depending on your platform.

And when you'll start RTView again, it will ask you:

```
Saved configuration file is found. Do you want to use it? <Y/N>
```

`Y` is a default option, so if you press `Y` or `Enter`, the saved configuration will be used. Otherwise, you will enter into the same interactive dialog again.

## Configuration file: example

This is an example of a saved RTView configuration created on Linux, with all default values accepted during dialog:

```
{
  "rotation": null,
  "defaultBackends": [
    "KatipBK"
  ],
  "setupBackends": [
    "KatipBK",
    "LogBufferBK",
    "TraceAcceptorBK"
  ],
  "hasPrometheus": null,
  "hasGraylog": null,
  "hasGUI": null,
  "traceForwardTo": null,
  "traceAcceptAt": [
    {
      "remoteAddr": {
        "tag": "RemoteSocket",
        "contents": [
          "0.0.0.0",
          "3000"
        ]
      },
      "nodeName": "node-1"
    },
    {
      "remoteAddr": {
        "tag": "RemoteSocket",
        "contents": [
          "0.0.0.0",
          "3001"
        ]
      },
      "nodeName": "node-2"
    },
    {
      "remoteAddr": {
        "tag": "RemoteSocket",
        "contents": [
          "0.0.0.0",
          "3002"
        ]
      },
      "nodeName": "node-3"
    }
  ],
  "defaultScribes": [
    [
      "StdoutSK",
      "stdout"
    ]
  ],
  "options": {
    "mapBackends": {
      "cardano-rt-view.acceptor": [
        "LogBufferBK",
        {
          "kind": "UserDefinedBK",
          "name": "ErrorBufferBK"
        }
      ]
    }
  },
  "setupScribes": [
    {
      "scMaxSev": "Emergency",
      "scName": "stdout",
      "scRotation": null,
      "scMinSev": "Notice",
      "scKind": "StdoutSK",
      "scFormat": "ScText",
      "scPrivacy": "ScPublic"
    }
  ],
  "hasEKG": null,
  "forwardDelay": null,
  "minSeverity": "Info"
}
```

## Explicit configuration

It is possible to provide a configuration file explicitly, via `--config` command line parameter:

```
cardano-rt-view --config /path/to/your/cardano-rt-view.json
```

## Logging

RTView has its log files. You can find the directory path where log files will be stored in `RTView Info` modal window: click to info icon at the top bar. Another way to find this path is to check RTView configuration file: see the section

```
"FileSK","/full/path/to/cardano-rt-view-logs/cardano-rt-view.log"
```

Please note that the minimum severity level for logging is `Info` by default. If you need to see detailed debugging information in the log, please set the minimum severity level to `Debug`. To do it, open RTView configuration file and change the line

```
"minSeverity":"Info"
```

to

```
"minSeverity":"Debug"
```

Changelog
=========

## [0.2.0]

### Added

- New CLI parameters `--version` and `--supported-nodes` ([#77](https://github.com/input-output-hk/cardano-rt-view/pull/77), [#83](https://github.com/input-output-hk/cardano-rt-view/pull/83))
- New `Idle` tag for the node ([#82](https://github.com/input-output-hk/cardano-rt-view/pull/82))
- New field `Blockchain start time` ([#96](https://github.com/input-output-hk/cardano-rt-view/pull/96))
- New field `Node start time` ([#97](https://github.com/input-output-hk/cardano-rt-view/pull/97))
- Now RTView knows the nodes it can work with: check/show the list of supported versions ([#83](https://github.com/input-output-hk/cardano-rt-view/pull/83))
- New features in `Errors` tab: sorting, filtering, exporting, removing ([#87](https://github.com/input-output-hk/cardano-rt-view/pull/87))

### Fixed

- UTF-8 encoding in Windows terminal is setting automatically ([#66](https://github.com/input-output-hk/cardano-rt-view/pull/66))
- Dynamic Y-axis for 'Resources' charts ([#86](https://github.com/input-output-hk/cardano-rt-view/pull/86))
- Fixed `Node commit` link ([#103](https://github.com/input-output-hk/cardano-rt-view/pull/103))

### Changed

- New way how RTView receives node's basic info ([#75](https://github.com/input-output-hk/cardano-rt-view/pull/75)). **IMPORTANT**: the node's version must be `1.22.1` or higher to work with it properly.
- Networking sockets connection is used by default during interactive dialog ([#80](https://github.com/input-output-hk/cardano-rt-view/pull/80))
- Suggested examples of the node's configuration: JSON instead of YAML ([#79](https://github.com/input-output-hk/cardano-rt-view/pull/79))
- Optimized web-page traffic: update DOM-elements only if needed ([#84](https://github.com/input-output-hk/cardano-rt-view/pull/84))
- Documentation improvements ([#62](https://github.com/input-output-hk/cardano-rt-view/pull/62), [#81](https://github.com/input-output-hk/cardano-rt-view/pull/81))
- Internal refactoring ([#76](https://github.com/input-output-hk/cardano-rt-view/pull/76), [#85](https://github.com/input-output-hk/cardano-rt-view/pull/85), [#104](https://github.com/input-output-hk/cardano-rt-view/pull/104))
- Moved to GHC 8.10.2 ([#65](https://github.com/input-output-hk/cardano-rt-view/pull/65))

## [0.1.0]

This is the first release of RTView.

### Added

- Cardano-like re-design ([#29](https://github.com/input-output-hk/cardano-rt-view/pull/29))
- Interactive dialog ([#12](https://github.com/input-output-hk/cardano-rt-view/pull/12), [#28](https://github.com/input-output-hk/cardano-rt-view/pull/28), [#49](https://github.com/input-output-hk/cardano-rt-view/pull/49))
- Documentation ([#37](https://github.com/input-output-hk/cardano-rt-view/pull/37), [#56](https://github.com/input-output-hk/cardano-rt-view/pull/56)).

### Fixed

- Fixed distributed work mode ([#50](https://github.com/input-output-hk/cardano-rt-view/pull/50))
- Improved GUI UX ([#27](https://github.com/input-output-hk/cardano-rt-view/pull/27), [#30](https://github.com/input-output-hk/cardano-rt-view/pull/30), [#33](https://github.com/input-output-hk/cardano-rt-view/pull/33), [#38](https://github.com/input-output-hk/cardano-rt-view/pull/38), [#39](https://github.com/input-output-hk/cardano-rt-view/pull/39), [#48](https://github.com/input-output-hk/cardano-rt-view/pull/48))
- Node uptime with days ([#26](https://github.com/input-output-hk/cardano-rt-view/pull/26))
- Fixed suggested pipe on Windows ([#25](https://github.com/input-output-hk/cardano-rt-view/pull/25))
- Corrected peers number ([#24](https://github.com/input-output-hk/cardano-rt-view/pull/24))

### Changed

- No changes yet, it is the first release.

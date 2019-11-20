# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.0] - 2019-11-20
### Changed
- Update CircleCI publishing strategy [#107](https://github.com/geotrellis/maml/pull/107)
- Upgrade to GeoTrellis 3.1.0 [#110](https://github.com/geotrellis/maml/pull/110)

### Fixed
- Manually create staging repository to define repository ID in state [#109](https://github.com/geotrellis/maml/pull/109)

## [0.4.0] - 2019-05-23
### Added
- Added `ParallelInterpreter` [#101](https://github.com/geotrellis/maml/pull/101)
- Add automated releases to Sonatype Nexus through CI [#98](https://github.com/geotrellis/maml/pull/98)

### Fixed
- Fixed 2.12 compilation in tests [#95](https://github.com/geotrellis/maml/pull/95)
- Reconcile change log for 0.3.3 release [#102](https://github.com/geotrellis/maml/pull/102)

## [0.3.3] - 2019-04-29
### Added
- Add README [#92](https://github.com/geotrellis/maml/pull/92)
- Add STRTA and migrate to CircleCI [#93](https://github.com/geotrellis/maml/pull/93)
- Add changelog and pull request template [#96](https://github.com/geotrellis/maml/pull/96)

## [0.3.2] - 2019-04-17
### Added
- Add hillshade [#77](https://github.com/geotrellis/maml/pull/77)

### Changed
- Upgrade circe version to 0.11.1 [#78](https://github.com/geotrellis/maml/pull/77)
- Use methods to create key decoders [#79](https://github.com/geotrellis/maml/pull/79)
- Mask on LazyMultibandRasters directly [#89](https://github.com/geotrellis/maml/pull/89)
- Assume correct extents in binary ops to avoid floating point errors [#91](https://github.com/geotrellis/maml/pull/91)

[Unreleased]: https://github.com/geotrellis/maml/compare/0.4.0...HEAD
[0.4.0]: https://github.com/geotrellis/maml/compare/0.3.3...0.4.0
[0.3.3]: https://github.com/geotrellis/maml/compare/0.3.2...0.3.3
[0.3.2]: https://github.com/geotrellis/maml/compare/v0.2.2...0.3.2

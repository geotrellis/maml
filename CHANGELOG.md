# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Expose Convert and InterpretAs MAML nodes [#125](https://github.com/geotrellis/maml/pull/125)

## [0.6.1] - 2020-06-23
### Added
- Add TargetCell Parameter to Focal operations [#118](https://github.com/geotrellis/maml/pull/118)
- Expose Focal Slope and Hillshade zFactor [#119](https://github.com/geotrellis/maml/pull/119)
- RGB composite as MAML operation, Rescale and Normalize nodes [#114](https://github.com/geotrellis/maml/issues/114)
- Receive GPG key while publishing artifacts [#122](https://github.com/geotrellis/maml/issues/122)
- Assemble bands into image as MAML operation [#123](https://github.com/geotrellis/maml/pull/123)

## [0.6.0] - 2020-02-06
### Added
- Added support for Focal Aspect calculations [#103](https://github.com/geotrellis/maml/issues/103)

### Changed
 - GeoTrellis 3.2.0 and bump dependencies for scala 2.12 build [#112](https://github.com/geotrellis/maml/pull/112)

## [0.5.1] - 2019-11-21

**No changes -- testing release process**

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

[Unreleased]: https://github.com/geotrellis/maml/compare/0.6.1...HEAD
[0.6.1]: https://github.com/geotrellis/maml/compare/0.6.1...0.6.0
[0.6.0]: https://github.com/geotrellis/maml/compare/0.5.1...0.6.0
[0.5.1]: https://github.com/geotrellis/maml/compare/0.5.0...0.5.1
[0.5.0]: https://github.com/geotrellis/maml/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/geotrellis/maml/compare/0.3.3...0.4.0
[0.3.3]: https://github.com/geotrellis/maml/compare/0.3.2...0.3.3
[0.3.2]: https://github.com/geotrellis/maml/compare/v0.2.2...0.3.2

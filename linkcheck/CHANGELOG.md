# Changelog

## [0.3.0.0] - 2026-03-08

### Added

* Check URLs in `<script>`, `<iframe>`, `<video>`, `<audio>`, `<source>`, `<embed>`, and `<object>` tags.
* Check `<video poster>` attribute for broken URLs.
* Check `<form action>` attribute for broken URLs.
* Parse `srcset` attributes on `<img>` and `<source>` tags for broken URLs.

## [0.2.0.0] - 2026-03-08

### Added

* Check `<meta>` tags with `property="og:image"` or `name="twitter:image"` for broken image URLs.

## [0.1.0.0] - 2022-05-09

### Added

* Linkcheck now caches requests when fragment checking is turned on.


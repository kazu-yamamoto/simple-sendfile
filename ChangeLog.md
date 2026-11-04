# ChangeLog for simple-sendfile

## 0.2.33

GHC 9.2 or later is required.

* Don't send the entire file for zero-length ranges on BSD/macOS
  [#43](https://github.com/kazu-yamamoto/simple-sendfile/pull/43)
* Fallback: open the file before sending the header
  [#44](https://github.com/kazu-yamamoto/simple-sendfile/pull/44)
* Reject negative offsets and lengths in FileRange
  [#45](https://github.com/kazu-yamamoto/simple-sendfile/pull/45)
* Fix the header path of BSD/macOS (IOV_MAX, EINTR, hook)
  [#47](https://github.com/kazu-yamamoto/simple-sendfile/pull/47)
* Linux: open the file before the header, handle EINTR and 64-bit off_t
  [#48](https://github.com/kazu-yamamoto/simple-sendfile/pull/48)
* Document how FileRange is handled
  [#49](https://github.com/kazu-yamamoto/simple-sendfile/pull/49)
* Add FreeBSD to CI and fix BSD error messages
  [#50](https://github.com/kazu-yamamoto/simple-sendfile/pull/50)

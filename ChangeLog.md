# Revision history for octar

## 0.3.1.1 -- 2018-09-10

* Fix bug caused by git-syncing

## 0.3.1 -- 2018-08-30

* Add `browse` command to browse non-local indexes
* Commands now do a git-pull or git-push as necessary (disable with -x)
* Move CLI definition into library

## 0.3.0.1 -- 2018-08-21

* Links in org-mode directories are properly %-encoded

## 0.3.0 -- 2018-08-15

* Org-mode directory view is no longer git-controlled.
* Added `refresh` command to rebuild directory views.
* Config file (`.octar`) allows multiple indexes and IPFS APIs.
* Added `rm` command for removing entries.
* Added `archivist` field to metadata, to track who added entries in a
  group-maintained archive.

## 0.2.1 -- 2018-06-24

* Add `-r` flag to `cp` method to allow storing directories of files.

## 0.2.0 -- 2018-05-14

* Change to multi-command CLI.
* Move main functionality to `add` command.
* Add `pin` command to replace the `pin-all` script.
* Enable modifying index directory by environment variable.
* Add git management for the index

## 0.1.1 -- 2018-04-17

* Add multiple "methods" for fetching source paths.

## 0.1.0.0  -- 2018-04-16

* Implement fetching and storing URLs with prompt for item synopsis.
  Links to items are added to an org-format directory when they are
  stored.

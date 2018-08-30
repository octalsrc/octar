# `octar` - a replicated archive organizer #

`[v0.3.1]`

`octar` is a tool for systematically fetching and labeling data from a
variety of sources.  It uses IPFS as a storage backend and git as an
index manager, which together provide:

1. Duplication avoidance
2. Easy replication/backups
3. Transparent access from multiple machines


## Usage ##

The primary function of `octar` is to copy a file from some source and
store it with contextual information that makes it findable and usable
in the future.

    $ octar add -nm "Some paper" https://www.cl.cam.ac.uk/~nk480/bidir.pdf

This saves the file, alongside a YAML-format metadata file, 

    sources:
    - date: 2011-04-16T22:55:51
      source: https://arxiv.org/pdf/1707.01747.pdf
    date: 2011-04-16T22:55:51
    message: Some paper
    archivist: joe
    main: 1707.01747.pdf

in IPFS, adding its link to an index of archived items.


There are several other commands for managing and browsing archives,
which you can learn about with `octar -h`.


## Future work ##

- An index editing interface that allows one-step removal of items
- Smarter shared-index manager than git, which would allow items added
  in parallel without a manual merge
- Index browser that supports filtering on keywords and opening,
  refiling, or removing selected items

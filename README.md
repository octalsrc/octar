# `octar` - a replicated archive organizer #

`[v0.3.1]`

`octar` is a tool for systematically fetching and labeling data from a
variety of sources.  It uses IPFS as a storage backend and git as an
index manager, which together provide:

1. Duplication avoidance
2. Easy replication/backups
3. Transparent access from multiple machines


## Setup ##

The first thing you need is an IPFS daemon that you can connect to.
If you don't have this already, go get [ipfs][1] from your package
manager or wherever and run

    $ ipfs --init daemon

Next you need a git-controlled index directory for your archive.

    $ git init .my-archive-repo

Finally, reference these in `octar`'s config file, which must be
placed at `$HOME/.octar`.

    # Here is an example config file, in YAML format
    
    default: my-archive

    indexes:

      my-archive:
        path: /FULL/PATH/TO/.my-archive-repo
        api: /ip4/127.0.0.1/tcp/5001
        archivist: "Alice"

        directories:
          - path: /FULL/PATH/TO/my-archive.org
            gateway: http://localhost:8080

Some explanation:

- `default`: You can have multiple indexes, which you choose via
  `octar`'s `-i` option.  If you leave the option out, `default` is
  used.
- `api`: This is the connection point for IPFS, which is on port 5001
  by default.  (If you are using the default, you can actually leave
  this field out)
- `archivist`: The name tacked onto the items you archive.  This is
  only really useful if you have a group-contributed archive, but it
  has to be set to something either way.  Think of it like a `git`
  author name.
- `directores`: Whenever you edit this index, a browsable "directory"
  will be written to these locations.  Currently, this is in the form
  of an [Org mode](https://orgmode.org/) file that is nicely rendered
  by Emacs.  It makes reachable URLs for archive entries by adding
  `gateway` to their paths.  If you don't specify the gateway, it
  defaults to `https://ipfs.io`, the globally usable (but possibly
  slow) IPFS gateway.  Your IPFS daemon by default provides you with a
  much faster gateway on port 8080.


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

in IPFS, adding its link to an index of archived items.  You will now
find this item listed in the directory file location you put in your
config file.  Using Emacs's Org mode, you can open the item by
clicking on its message.

    $ emacs /FULL/PATH/TO/my-archive.org


There are several other commands for managing and browsing archives,
which you can learn about with `octar -h`.


## Future work ##

- An index editing interface that allows one-step removal of items
- Smarter shared-index manager than git, which would allow items added
  in parallel without a manual merge (something like a CRDT, probably
  [card-systems][2] when it's ready)
- Index browser that supports filtering on keywords and opening,
  refiling, or removing selected items


[1]: https://docs.ipfs.io/introduction/install/
[2]: https://github.com/cuplv/card-systems

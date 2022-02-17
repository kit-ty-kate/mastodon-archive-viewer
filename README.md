## How to install mastodon-archive-viewer

First you need an initialized OPAM installed
If you don't have it installed, it can be fetched from your system packages manager.
Then to initialize it, do `opam init -a`, then `eval $(opam config env)`

Now, to install mastodon-archive-viewer, clone this repo and do:

```sh
opam pin add mastodon-archive-viewer <the repo directory>
```

## How to use mastodon-archive-viewer

Now that mastodon-archive-viewer is installed you just need to download the archive you want
to view, extract it in a new directory, then do:

```sh
cd <the archive directory>
mastodon-archive-viewer outbox.json
```

The mastodon-archive-viewer tool has also some options to filter out some items for example. Check out
the available options with `mastodon-archive-viewer --help`

## Troubleshootings

If the installation fails somewhere while building the dependencies, you might miss one of
these tools on your systems: make, gcc, m4, libc-dev.
This might happen on a newly installed Alpine Linux, for instance.

## TODO

* Testing (I guess I missed some heuristics, help welcome!!)
* Documentation
* Clean code and better errors

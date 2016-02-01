# dblp2bibtex

**Note** This program depended on the Sindice web service. This
  service was [shut down in May
  2014](http://www.dataversity.net/end-support-sindice-com-search-engine-history-lessons-learned-legacy-guest-post/). For
  this dblp2bibtex program to work again, an alternative web service
  offering a similar interface to DBLP records will need to be
  saught. Pull requests welcome!

This `dblp2bibtex` tool provides 3 features, primarily generating bibtex files for authors identified in the DBLP database. Authors are disambiguated with Unique Resource Identifiers, which are used extensively in this utility.

* Search for a DBLP URI given a name (e.g. "Simon Peyton Jones")
* List the titles of publications for a give author URI
* Generate a bibtex file for all publications authored by the given author URI

## Usage

```
dblp2bibtex [OPTIONS]
  A Haskell utility to generate bibtex files for an author identified with a DBLP URI

Common flags:
  -g --generatebibtex=Author URI  Get bibtex file for given URI
  -s --search=Author name         Search for URI by name (e.g. "Joe Bloggs")
  -l --listpapers=Author URI      List papers for an author URI
  -o --outfile=Bibtex filename    (default 'export.bib')
  -x --xref                       Include cross reference entries
  -h --help                       Display help message
  -v --version                    Print version information
```

### Examples

```
$ dblp2bibtex -s "Simon Marlow"
http://dblp.l3s.de/d2r/resource/authors/Simon_Marlow

$ dblp2bibtex -l http://dblp.l3s.de/d2r/resource/authors/Simon_Marlow
Developing High-Performance Server Applications in Haskell - Case Study: A Haskell Web Server.
A Semantics for Imprecise Exceptions.
Deforestation for Higher-Order Functions.
Composable memory transactions.
...

$ dblp2bibtex -x -g http://dblp.l3s.de/d2r/resource/authors/Simon_Marlow -o papers.bib
```


## Installation

The Haskell platform is needed to resolve dependencies and to install `dblp2bibtex`. It can be downloaded easily using package managers on most Linux distributions, or directly from [Haskell Platform](http://hackage.haskell.org/platform/).

    cabal update
    cd ~/path-to-install
    git clone git://github.com/robstewart57/dblp2bibtex.git
    cd dblp2bibtex
    cabal install


## Issues?

This can be regarded as experimental software, and will probably have edge case bugs. Contributions are welcome! Please report issues on the GitHub issues: [issues](https://github.com/robstewart57/dblp2bibtex/issues).

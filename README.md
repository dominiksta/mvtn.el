# mvtn - "Minimum Viable Text Notes"

[![Unit-Tests](https://github.com/dominiksta/mvtn.el/actions/workflows/testing.yml/badge.svg)](https://github.com/dominiksta/mvtn.el/actions/workflows/testing.yml)
[![License](https://img.shields.io/github/license/dominiksta/mvtn.el)](https://github.com/dominiksta/mvtn.el/blob/master/LICENSE)

**WARNING**: This package is yet to be tested thouroughly and is still subject
to breaking changes. Do NOT expect stability just yet. A first release is coming
soon™ (or not).

Mvtn is a plain-text note taking system for emacs. The primary guiding principle
is simplicity. The core of mvtn should *always* remain independent of add-on
code and as small as reasonable in LOC.

I will write more thourough documentation on release.

## Features

- Mvtn is independent of file format. You can use Markdown, org-mode or whatever
  format you want! (see `mvtn-default-file-extension` and
  `mvtn-file-extension-templates`)
- **Backlinks** and full text search with `ag` (the silver-searcher) or `rg`
  (ripgrep) (see `mvtn-search-backlinks`, `mvtn-search-full-text` and
  `mvtn-search-function`)
- View all notes by tag (see `mvtn-tag-file-list`)
- Keep a controlled vocabulary of tags (see `mvtn-cv-file`)
- Journaling (see `mvtn-journal-new-(quick-)entry`)
- **Automated journaling** (see `mvtn-journal-autojournal-set-feature`): logging git
  commits, note edits and org-clock events
- Integration with **org-agenda** (see `mvtn-org-agenda`)

## Core Concepts and Limitations

To use mvtn effectively, it is necessary to understand some of its core
concepts:

- Every note in mvtn has an "id". This is really just a timestamp that is
  accurate to the second and prefixed to every notes file name. This id _must
  never be changed_, because it is used to define links. So a change in the id
  would break a link. The advantage of using ids is that you can freely rename a
  note as you please and it will not break links.
- Mvtn **does not use a database index**. This is a good thing because it makes
  the code _much_ simpler and independent of other projects and it reduces sync
  conflicts with tools like Syncthing, Nextcloud, Dropbox, etc. There are some
  downsides to this approach though:
  - Mvnt can not work with arbitrary folder structures. You will have to define
    a strict folder structure in your configuration and limit yourself to as few
    folders as possible to ensure scalability.
  - Notes will be placed in directories for the year a note is taken in. This
    allows mvtn to ignore old notes by default, making it possible to scale far
    beyond what is normally possible without an index (see `mvtn-search-years`)
  - Generating a graph view like that of Obisidian or similar tools is not
    supported. I _might_ implement this in the future, but it would be a
    seperate project that would add a datase index to mvtn. I don't really plan
    on doing that though, so if you need a graph view, then mvtn is not for you.

## Todo-list before release

- [ ] Write a thourough documentation in texinfo format
- [ ] Performance test with a large amount of generated notes. While the
      performance with my personal notes is fine (~200 notes), this is not a
      good indication of performance.
- [ ] Fail more gracefully when a single note does not have a proper id

## Installation

While a release on melpa is eventually planned, this will only happen after the
first stable release. If you are interested in trying out the project now, I
recommend you use [straight.el](https://github.com/raxod502/straight.el):

```elisp
(straight-use-package '(mvtn :type git :host github :repo "dominiksta/mvtn.el"))
```

Alternatively, you can clone the repository and build a tarball package with
`make package` and then install that file through `package-install-file`.

## Configuration

The most simple configuration only needs to specify the directory structure of
your notes. You can refer to the docstring of `mvtn-note-directories` for this.

As an example, the following snippet will set up two note directories - one for
private notes and one for work related notes. Each note directory will have
several subdirectories for different categories of notes.

Be warned: Having too many different directories (especially non-datetree
directories) _will_ slow things down.

```elisp
(setq mvtn-note-directories
      '((:dir "~/sync/documents/notes/mvtn" :name "prv" :structure
              ((:dir "flt" :datetree t) ;; fleeting
               (:dir "lit" :datetree t) ;; literature
               (:dir "tec" :datetree t) ;; tech (devlog, etc.)
               (:dir "stc" :datetree nil))) ;; static
        (:dir "~/Documents/work/notes" :name "wrk" :structure
              ((:dir "flt" :datetree t)
               (:dir "tec" :datetree t)
               (:dir "stc" :datetree nil))))) ;; static
```

For more advanced configuration, you can refer to my (@dominiksta) [personal
config](https://github.com/dominiksta/dotfiles2/blob/master/stow/emacs/.emacs.d/config/applications/config-mvtn.el).

## Acknowledgements

Mvnt borrows heavily from several existing emacs packages:
- [usls](https://protesilaos.com/codelog/2020-10-08-intro-usls-emacs-notes/)
- [org-roam](https://github.com/org-roam/org-roam)
- [zetteldeft](https://github.com/EFLS/zetteldeft)

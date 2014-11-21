# ace-link

**GNU Emacs package for selecting a link to jump to**

## What and why

Currently to jump to a link in an `Info-mode` or `help-mode` buffer,
you can tab through the links to select the one you want.  This is an
O(N) operation, where the N is the amount of links.  This package
turns this into an O(1) operation, or at least O(log(N)) if you
manage to squeeze thousands of links in one screen:).
It does so by assigning a letter to each link using `ace-jump-mode`.

## Install
Either clone from here or install from [MELPA](http://melpa.milkbox.net/) (recommended).

## Setup

Put this in your `~/.emacs`:

    (ace-link-setup-default)

This will bind `ace-link-info` and `ace-link-help` to `o` in their
respective modes. This shortcut was previously unbound and is very
close to `l` which is the shortcut to go back.

To bind `ace-link-org`, use something like this:

    (define-key org-mode-map (kbd "M-o") 'ace-link-org)

If you use `ert`, `ace-link-help` also works on `ert` results:

    (require 'ert)
    (define-key ert-results-mode-map "o" 'ace-link-help)

## Usage

Just press `o` when you're in `Info-mode` or `help-mode`.

Here's a screencast of browsing Info using `ace-link-info`:

![gif][screencast-1]

[screencast-1]: https://raw.githubusercontent.com/abo-abo/ace-link/gh-pages/screencast-1.gif

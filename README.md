# ace-window

**GNU Emacs package for selecting a link to jump to**

## What and why

Currently to jump to a link in an `Info-mode` or `help-mode` buffer,
you can tab through the links to select the one you want.  This is an
O(N) operation, where the N is the amount of links.  This package
turns this into an O(1) operation, or at least O(log(N)) if you
manage to squeeze thousands of links in one screen:).
It does so by assigning a letter to each link using `ace-jump-mode`.

## Setup

Just call `ace-link-setup-default` to bind `ace-link-info` and `ace-link-help` to
`o` in their respective modes. This shortcut was previously unbound and is very
close to `l` which is the shortcut to go back.

## Usage

Just press `o` when you're in `Info-mode` or `help-mode`.

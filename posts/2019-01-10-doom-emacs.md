---
title: Doom Emacs Workflows
---

*Last update: 15 February 2020*

I recently switched to [Doom Emacs][doom] from Spacemacs. The Doom documentation is currently quite sparse, so I've posted here my personal notes on using Doom along with a description of my workflow (something I find vital but missing from most documentation). Doom might be for you if

- you like Emacs but want to use Vim keybindings;
- you don't have time to configure all the nice libraries like `helm`; and
- you find Spacemacs too slow.

I'm not going into detail on how Emacs works or basic Vim keybindings here---the focus is on the things I found different and useful.

<!--more-->

## Installation

You definitely want to use the `develop` branch, not `master`! This means:

```bash
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
cd ~/.emacs.d
git checkout develop
```

Then run `bin/doom quickstart` to get everything setup. Run `bin/doom help` to see what other commands are available.


## Workflow: Projects and Workspaces

I find it useful to manage my work with projects and workspaces. I get two benefits from this: it's faster to find the things I'm looking for as I only look within the current project, and it's faster to get to work as I can save and restore window and buffer configuration. In Doom this means using [projectile][projectile] and [persp-mode][persp-mode]. 

The basic workflow is:

- Decide what I'm working on. Is this a distinct project, or is it something that spans multiple projects (such as managing my todo lists) or doesn't need to live in a project (such as doing a quick calculation in a scratch buffer)? If it's a distinct project it gets it's own workspace. If not I'll do it in the default workspace (`main`).

- Switch to the appropriate workspace with `SPC TAB .` If the appropriate workspace is not loaded load it with `SPC TAB l`. Otherwise create a new workspace with `SPC TAB n`. If I'm creating a new workspace I will often give it a more informative name, using `SPC TAB r` and then save with `SPC TAB s`.

- If I'm switching to an existing workspace Emacs will be returned to the state it was in when I last worked on the project and I can get straight to work. If not I'll select a project for this workspace using `SPC p p` and then get to work.

Three very useful commands are:

- `SPC SPC` to switch to a file within the current project, with fuzzy completion; and
- `SPC ,` to switch to a buffer within the current project, again with fuzzy completion.
- `SPC fr` to load a recently viewed file.


## Common Tasks

Things I do all the time:

- `SPC :` for `M-x`.

- `SPC gg` for [Magit][magit], which is the only sane way to use Git.


Things I occasionally do:

- `SPC .` to find a file. Using `SPC SPC` is faster, so try to use `SPC .` only when you want to switch to something outside the current project. 

- `SPC b B` to switch to a buffer outside the current project.


## Finding Text

If you know there is text somewhere, but you don't know where, there are two basic ways to find it:

- `SPC sp` to search all files in the current project. (Press `SPC s` and wait for a popup for other options.) When you have a buffer of matches you can jump to the one your want by pressing return or press `C-c C-e` to edit *all* the files at once. You can then do whatever edits you want and press `C-c C-c` to commit, or `C-c C-k` to abandon your changes. This is super useful for both simple and complex edits across multiple files.

- `/` to search within the current buffer. Use `n` and `N` to go to next and
  previous matches. (`?` to search backwards.)


## Moving To Text

If you can see text on the screen and want to move to it there are a number of ways to quickly get there. 

- `s` and type two characters to jump forward to the nearest match, and `S` to jump backwards. Type `,` to cycle to matches earlier in the buffer and `;` to cycle through matches later in the buffer. By default these functions only jump to a match on the current line, but if you cycle with `,` and `;` they'll jump around the whole buffer. This is [evil-snipe][evil-snipe], which has an amazing cover graphic / logo.

- `gs SPC`, start typing the word you're looking at. You'll either jump directly there or be given a quick key to press to disambiguate the phrase you've typed. If you add `(setq avy-all-windows t)` to your `config.el` this will work across all visible windows. I love this method for quickly jumping around the screen. This uses [Avy][avy].

- `gs` and wait for the popup for a million other search methods. I don't really use them as the two options above do everything I need, but maybe I'm missing out.

If the things I want to jump to is fairly close to where the cursor is I'll use evil-snipe (`s` and `S`). If it's far away or in another window I'll use Avy (`gs SPC`). Both can be used to create text objects. Imagine we have the text

```
Just some example text
```

with the cursor at the `J` and we want to select `Just some example`. We could use `v3e` but we have to count the number of words (and remember the difference between `e` and `w`). Instead we could type `vsle`. The `s` uses evil-snipe, and `le` is the two characters we're sniping. We could also use `vgsSPCle` to use Avy (and we'd then have to move the cursor one character right). 

There is a bit of complexity using evil-snipe. It's bound to `s` for visual selection, but other commands (e.g. yank or `y`) bind `s` to [evil-surround][evil-surround]. In these cases evil-snipe is bound to `z` (or `Z` to snipe backwards). If we don't want include the letters we're sniping in the text object we can use `x` (or `X` for backwards sniping).


## Narrowing and Widening Regions

It can be useful to restrict a buffer to a selection of text, called narrowing in Emacs. This is particularly useful with multiple cursors (see below). To narrow to the current selection use `SPC rn` and use `SPC rw` to perform the reverse, called widening. Narrowing to the current function, with `SPC rf`, is also useful.


## Multiple Cursors

Multiple cursors allow you to edit multiple things at a time. Doom provides two implementations of multiple cursors, [evil-multiedit][evil-multiedit] and [evil-mc][evil-mc]. I find evil-multiedit easier to use, but it is less powerful.

There are two ways to start editing with evil-multiedit:

- select one example of the things you want to edit and press `R` to select all the rest in the current buffer (perhaps narrowing first); or

- move the cursor over one example of the word you want to edit and then repeatedly press `M-d` and `M-D` to select next or previous examples respectively.

Having selected some regions you can move between them using `C-n` (next) and `C-p` (previous). Pressing `RET` on a region will remove it from the selection, so by using these commands you can choose arbitrary regions to include.

Once you have selected the regions, edits made to one region will be reflected in all the others. The majority of evil commands work with evil-multiedit.

Finally use the usual cancel key (`ESC` or `C-[`) to exit out of evil-multiedit.


The other multiple cursor implementation, evil-mc, is a bit more flexible but
harder to use. There are more options for creating multiple cursors with evil-mc:

- `gzm` to create cursors at all matches for the word at point (possibly
  narrowing the buffer first);
- `gzd` to create a cursor at point and move to the next match (and `gzD`
  to create and move to the previous match);
- `gzj` to create a cursor at point and move to the next line (and `gzk` to
  create and move to the previous line); or
- `gzz` to create a cursor at point.

Cursors will mirror the commands you enter at the "real" cursor. You can temporarily turn off multiple cursors with `gzt`. This is automatically done if you create a cursor at point with `gzz`. Start the cursors again with the same key combo, `gzt`.

Most commands work with evil-mc but a few commands I commonly use do not:

- `DEL` (backspace) in insert mode will not be repeated across all cursors in
  some modes; and
- `ysiw` doesn't work across all cursors in all the modes I tried (though `ciw`, for example, does).

If the cursors get out of sync undoing a few commands usually sorts things out.

When you've finished with multiple cursors press `gzu` to remove them all.


## Undoing and Redoing

*TODO*

`u` is undo, but this uses the `undo-tree` system which is a bit more
complicated that you might be used to.
`C-r` and `M-_` is redo
`C-x u` shows the undo tree.


## Navigating Source Code

*TODO*

`gd` or `SPC cd` to go to the definition of the symbol at point.
`gD` or `SPC cD` to list references to the symbol at point.
`K` to lookup documentation for the symbol at point. Uses Dash docsets if you
have them installed and the major mode is correctly configured.


## Navigating Compilation Output

*TODO* 

`SPC p c` or `M-b` to compile (build)
`]e` and `[e` for next and previous flycheck error, respectively
`SPC c x` to list current errors (from flycheck)


## Miscellaneous

- `gcc` to comment or uncomment the current line or selection;
- `SPC oA` for the Org agenda;
- `SPC x` for a temporary buffer for random notes;
- to change text size in the current buffer: `M-=` to increase, `M--` to
  decrease, and `M-+` to reset it.

## Learning More About Doom

I learned most of what I know about Doom by reading through the [default
keybindings][doom-bindings] and looking up commands I didn't recognise. It's
easy to experiment in a scratch buffer to figure out what a command does.

Learning Doom has been a gradual process for me. I didn't adopt all the above at
once! In fact many things I only figured out when I came to write this document.


[avy]: https://github.com/abo-abo/avy
[doom]: https://github.com/hlissner/doom-emacs
[doom-bindings]: https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el
[evil-multiedit]: https://github.com/hlissner/evil-multiedit
[evil-mc]: https://github.com/gabesoft/evil-mc
[evil-snipe]: https://github.com/hlissner/evil-snipe
[evil-surround]: https://github.com/emacs-evil/evil-surround
[magit]: https://magit.vc/
[persp-mode]: https://github.com/Bad-ptr/persp-mode.el
[projectile]: https://github.com/bbatsov/projectile
[treemacs]: https://github.com/Alexander-Miller/treemacs

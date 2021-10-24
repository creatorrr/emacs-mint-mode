emacs mint mode
===============

Major mode for [mint lang](https://www.mint-lang.com).

Installing
----------

### Using [`use-package`](https://github.com/jwiegley/use-package) and [`straight.el`](https://github.com/raxod502/straight.el)

Add the following to your `~/.emacs` config and restart your editor:
```elisp
(use-package mint-mode
  :straight (mint-mode
             :type git
             :host github
             :repo "creatorrr/emacs-mint-mode"
             :files ("tokens" "mint-mode.el"))
  :mode ("\\.mint\\'" . mint-mode))
```

### Using [`leaf.el`](https://github.com/conao3/leaf.el) and [`Guix`](https://guix.gnu.org)

Install [leaf](https://github.com/conao3/leaf.el) and [emacs-mint-mode](https://github.com/creatorrr/emacs-mint-mode) to your default [profile](https://guix.gnu.org/cookbook/en/html_node/Guix-Profiles-in-Practice.html) with the following command:
```sh
guix install emacs-leaf emacs-mint-mode
```

Add the following to your `~/.emacs` or `~/.emacs.d/init.el`:
```elisp
(leaf mint-mode)
```

Features
--------

 - Syntax highlighting
 - Basic keyword autocomplete (using company-mode)
 - Auto format on save using `mint format`

Planned roadmap
---------------

__Help wanted with these!__

 - [x] Show formatting errors in buffer
 - [x] Highlight HTML tags and attributes
 - [x] Add to MELPA ([Pending review](https://github.com/melpa/melpa/pull/5816))
 - [x] Autocomplete support
 - [x] Tag a release
 - [x] Add to Guix ([Merged](https://issues.guix.gnu.org/51045))
 - [ ] Lint css values
 - [ ] Link identifiers to their location
 - [ ] Documentation search
 - [ ] Emacs shell commands for using the mint CLI
 - [ ] Language Server Protocol Integration and Testing ([In progress](https://github.com/joaotavora/eglot/pull/750))

Screenshots
-----------

![Mint syntax highlight example](img/sample1.png?raw=true "Mint syntax highlight")
![Mint syntax highlight example](img/sample2.png?raw=true "Mint syntax highlight")
![Mint auto formatting example](img/sample3.png?raw=true "Mint auto formatting")

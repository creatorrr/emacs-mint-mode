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

### Using [`leaf`](https://github.com/conao3/leaf.el)

Add the following to your `~/.emacs` or `~/.emacs.d/init.el`:
```elisp
(leaf mint-mode
  :mode ("\\.mint\\'" . mint-mode))
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
 - [ ] Tag a release
 - [ ] Add to [Guix](https://guix.gnu.org/)  
 - [ ] Lint css values
 - [ ] Link identifiers to their location
 - [ ] Documentation search
 - [ ] Emacs shell commands for using the mint CLI

Screenshots
-----------

![Mint syntax highlight example](img/sample1.png?raw=true "Mint syntax highlight")
![Mint syntax highlight example](img/sample2.png?raw=true "Mint syntax highlight")
![Mint auto formatting example](img/sample3.png?raw=true "Mint auto formatting")

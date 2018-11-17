emacs mint mode
===============

Major mode for [mint lang](https://mint-lang.com).

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
             :files ("mint-mode.el"))                                                                                                                                                          
  :mode ("\\.mint\\'" . mint-mode))                                                                                                                                                            
```

Features
--------

 - Syntax highlighting
 - Auto format on save using `mint format`

Planned roadmap
---------------

__Help wanted with these!__

 - Autocomplete support
 - Lint css values
 - Highlight HTML tags and attributes
 - Emacs shell commands for using the mint CLI
 - Documentation search
 - Add to MELPA

Screenshots
-----------

![Mint syntax highlight example](img/sample1.png?raw=true "Mint syntax highlight")
![Mint syntax highlight example](img/sample2.png?raw=true "Mint syntax highlight")

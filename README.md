# my_emacs_d

My Emacs configuration, the way I like it

Configuration compatible with Emacs version >= 28.x

## Installing

```
cd ~/projects
git clone https://github.com/rabbitonweb/my_emacs_d
cd
mkdir .emacs.d
cd .emacs.d
ln -s ~/projects/my_emacs_d/init.el ~/.emacs.d/init.el
ln -s ~/projects/my_emacs_d/configs ~/.emacs.d/configs
ln -s ~/projects/my_emacs_d/my_snippets ~/.emacs.d/my_snippets
ln -s ~/projects/my_emacs_d/logo.png ~/.emacs.d/logo.png
```

Also install Silver Search on your machine https://github.com/ggreer/the_silver_searcher

Also install mactex for Mac OS

```
brew cask install mactex
```


Also install ispell

```
brew install ispell
```

Also install direnv

```
nix-env -i direnv
```

## TODOs
* [Use org-mode as init file](https://gewhere.github.io/orgmode-emacs-init-file)
* [evil-mode vidual replace](https://github.com/troyp/evil-visual-replace#introduction)
* [spell & abbrev](https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html)
* [integrate org with google calendar](https://github.com/kidd/org-gcal.el)
* [integrate org with google tasks](https://github.com/JulienMasson/org-gtasks)
* [anzu](https://github.com/emacsorphanage/anzu)
* [evil org-mode](https://github.com/Somelauw/evil-org-mode)
* [org-appear](https://github.com/awth13/org-appear)
* [Chat-GPT](https://www.youtube.com/watch?v=fvBDxiFPG6I)
* [Better spelling with aspel (+camelCase support)](http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html)
* [Spellchecing with English grammar!](https://joelkuiper.eu/spellcheck_emacs)
* [elfeed customizations](https://nullprogram.com/blog/2015/1203/)
* Configure org to easly refile tasks Check possible [refile configuration](https://github.com/syl20bnr/spacemacs/issues/3094[discussion])
* [https://orgmode.org/manual/Exporting-Agenda-Views.html](Agenda Views)
* [https://github.com/marcinkoziej/org-pomodoro/tree/master](org-pomodoro) [https://www.youtube.com/watch?v=x6wbb0xeBTg](with nice setup)
* aspell
  https://github.com/NixOS/nixpkgs/issues/1000
* [https://orgmode.org/manual/Tracking-your-habits.html](habbits with org-mode)
  Also video https://www.youtube.com/watch?v=acj3NhXlnnk
* [change-case.el](https://github.com/TakesxiSximada/change-case.el)

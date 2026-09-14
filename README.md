<p align="right">
  <img width="200" height="200" alt="onstate-logo" src="https://github.com/user-attachments/assets/6ff9bf23-4686-4599-a011-200200a55914" />
</p>

# org-onstate

Run ordinary Emacs Lisp functions when an Org heading enters a TODO state.

## Quick start

```elisp
(use-package org-onstate
  :vc (:url "https://github.com/Seryiza/org-onstate"
       :rev :newest)
  :hook (org-mode . org-onstate-mode))
```

- Automatically enables the mode in Org buffers; use only with trusted files.
- This GitHub installation method requires Emacs 30.1+ and Git.

> **Security:** Properties execute named functions. This is **not a sandbox**.

Create and save `~/org/tasks.org`:

```org
* TODO Start laundry
:PROPERTIES:
:ON_DONE: (org-onstate-schedule :id "hang-laundry" :after "1h" :state "TODO")
:END:

* DONE Hang laundry
:PROPERTIES:
:ID: hang-laundry
:END:
```

Then configure and scan the file:

```elisp
(setq org-agenda-files '("~/org/tasks.org"))
(org-id-update-id-locations org-agenda-files)
```

Mark `Start laundry` DONE → `Hang laundry` becomes TODO, scheduled for one hour later.

Multiple actions also fit on one physical property line:

```org
:ON_DONE: ((message "Laundry started") (org-onstate-schedule :id "hang-laundry" :after "1h" :state "TODO"))
```

## Why org-onstate?

- **Any TODO keyword:** `ON_NEXT`, `ON_WAITING`, `ON_DONE` — not just completion.
- **Ordinary Elisp functions:** no separate action DSL; rules live on the heading.
- **Minimal scope:** Org-only core; separate scheduling helpers; no timers or workflow framework.

### Alternatives

- [org-edna](https://www.nongnu.org/org-edna-el/): dependencies, DSL, TODO → DONE triggers. **org-onstate:** any state entry, ordinary functions.
- [org-depend](https://orgmode.org/worg/org-contrib/org-depend.html): blockers, DONE-triggered task chains. **org-onstate:** per-state actions, no dependency rules.
- [Org tag triggers](https://orgmode.org/manual/TODO-Basics.html): built-in tag changes. **org-onstate:** function calls beyond tags.

Prefer Edna for dependency enforcement; built-in triggers for tags alone.

## Files

- `org-onstate.el`: core mode and dispatcher; enough for your own functions.
- `org-onstate-actions.el`: built-in actions, autoloaded on first use after package installation.

## Reference

- Exact local `ON_<STATE>` properties only; no inheritance.
- One function call or a list; literal arguments are not evaluated.
- Empty values, `nil`, and `()` do nothing.
- `org-onstate-schedule`: required `:id`/`:after`, optional exact `:state`.
- Delays use positive integers plus `m`, `h`, or `d` (`d` = 24 hours), start at
  event time, and produce minute-precision timestamps.
- Targets must be existing ID-resolved headings without repeaters.
- FIFO dispatch; `org-onstate-max-events` defaults to 100.
- Failures warn, stop pending work, and do not roll back completed effects.
- Repeater reset can trigger `ON_TODO` before Org advances its timestamp.
- Save edited buffers and press `g` to refresh an agenda.
- No timers, notifications, delayed visibility, or automatic saves.

## Tests and requirements

```sh
make check
make check EMACS=/path/to/emacs
```

Requires Emacs 28.1+ and Org 9.5+; tested with Emacs 30.2 / Org 9.7.11.

---

Developed by AI and me

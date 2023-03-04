# outli
Simple and stylish comment-based outlines for Emacs.
![image](https://user-images.githubusercontent.com/93749/190755666-69ca250c-476b-41c9-a26a-f9c12d167c99.png)

_outli_ is a simple Emacs outliner for code, documents, and more which provides heirarchical hide/show, styles your headings, and emulates org-mode navigation and structure editing.  It is based on the built-in `outline-minor-mode` and is simple by design, providing just a few key additional features beyond what outline already offers:

- Configurable heading syntax based on the concept of a `stem` (fixed first characters) and `repeat-char` (the number of which determines a heading's depth).  Example level-two headers include classics such as `;;;;` and `# **`, but anything's possible.
- Header style options including color-matched overline and blended background color for the initial heading info.
- `Tab` and `Shift-Tab` work just like you'd expect from org-mode to toggle headings or document visibility. 
- _Speed keys_ mirroring org-mode for easy navigation, visibility, and structure editing at headlines.  Hit `?` for the list of available keys.   Additions include `h` to hide sublevels below the current level, and `1`-`5` to specify such a level directly. 
- Exposes headings to imenu.  A fast imenu browser like [consult-imenu](https://github.com/minad/consult) is recommended. 

# Configuration
Not yet in a package database; simply clone and point `use-package` at the correct path (or use [straight](https://github.com/radian-software/straight.el), etc.).

```elisp
(use-package outli
  :load-path "~/code/emacs/outli"
  :after lispy ; only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer
```

You should probably not enable outli in org-mode (which by default is prevented), or with other packages which operate on headings, like outshine. 

# Customization
## Headline style
The main variable to customize is `outli-heading-config`, where you can set the _stem_ and _repeat char_, and influence the styling, including whether to style the stem and repeat chars the same, whether to include the overline, or whether to omit styling altogether.  The custom variable `outli-blend` controls whether a blended background is used to start the headline.  

Configured defaults are:

- **emacs-lisp-mode**: stem `;;`, repeat-char `;`
- **tex-mode**: `%%`, `%`
- **org-mode**: disabled
- **others**: `comment-start` + a space, `*`

## Speed keys
Customize `outli-speed-commands` to alter or disable speed keys, which work at the beginning of heading lines only.  

# FAQ
- **How does this relate to outline-minor-mode?**  
 
  _outli_ is mostly a convenient wrapper around functionality that is already built-in to outline, adding a few things like `narrow-to-subtree` and `insert-heading-respect-content` (ala org). And of course the speed-key bindings, automatic comments-as-header patterns, and styling.  
- **How does this relate to outshine?**  

  Mostly just conceptually.  Outshine also provides (different) speed keys, for example.  And a lot more, much of which isn't as relevant to modern emacs.  Since it builds more direclty on the built-in capabilities of outline-minor-mode, _outli_ is a _much_ smaller and simpler package. 

- **What is the syntax of `outli-heading-config`?** 

  It's an _alist_, each entry of which looks like:

    `(MAJOR-MODE STEM REPEAT-CHAR STYLE NO-BAR)` to configure a mode.
  or
  
    `(MAJOR-MODE . nil)` to explicity prevent outli from running in this mode.
	
  I recommend using the customize interface to configure outli: `M-x customize-group outli`.  But it may help to know:
    - `MAJOR-MODE`: A symbol for a major mode, or parent mode from which the current mode inherits, like `'text-mode` (note: omit the single apostrophe in the customize interface: it knows it's a symbol).
    - `STEM`: A string like `"# "`: the fixed "stem" of the headline pattern (omit quotes in customize interface).  Can also be an elisp expression which evaluates to a string.
    - `REPEAT-CHAR`: A _character_ like `?*`: the repeating character which specifies the level of a headline (again: no `?` needed in customize, just type the character).  Can also be an elisp expression which evaluates to a character. 
    - `STYLE`: A style flag: `nil` for default styling, `'none` for no styling, and `t` for uniform styling between stem and repeat char.  Can be omitted (defaults to `nil`).
    - `NO-BAR`: A flag for the overline bar: if non-`nil`, omit the overline.  Can be omitted (defaults to `nil`).

# Tips
- You can use arbitrary expressions for the stem and repeat chars; they'll get evaluated at run-time.
- It's useful to target high-level modes like prog-mode or text-mode from which many modes inherit (see [mode-minder](https://github.com/jdtsmith/mode-minder) to get a list of your major mode heirarchy).
- Try out the `h` key at headline start: it folds everything up to be no deeper than the current header's level. 
- To prevent outli from being enabled in a given mode (or family of derived modes), just include `(MODE . nil)` in `outli-heading-config`.  By default, org-mode is excluded in this way.

# Related Packages
- outline-minor-mode: The built-in minor mode for outlining documents on which _outli_ builds. 
- [orgmode](https://orgmode.org): The do-everything outliner mode.
- [outshine](https://github.com/alphapapa/outshine): A featureful outline-mode enhancement from which `outli` took its inspiration.  Has many legacy features. 
- [outorg](https://github.com/alphapapa/outorg): Required by outshine, this mode enables editing comment blocks in temporary org-mode buffers (the inverse of code-blocks in org).
- See [more related packages for org-like behavior outside of org](https://orgmode.org/worg/org-tutorials/org-outside-org.html). 

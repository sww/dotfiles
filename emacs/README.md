`emacs`
===

### Setup

Symlink `init.el` to the `~/emacs.d` directory.

```bash
ln -s /path/to/dotfiles/init.el ~/.emacs.d/init.el
```

### Troubleshooting

#### C-Spc Not Selecting Region

If `C-Spc` is not marking regions, try [disabling input sources that clobber C-Spc.](https://emacs.stackexchange.com/a/21311).

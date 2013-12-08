Introduction to sage-mode
=========================

Installation and setup
-----------------------
There are three possibilities for installing sage-mode:

### Option 1. Use the sage-mode bundled with Sage
This is the easiest. Since the spkg is already installed, you only need to
activate it in Sage and in Emacs. For the former, run

    sage -f sage_mode

For the latter, then somewhere in your .emacs add the following

    (add-to-list 'load-path "$INSTALL_DIR")
    (require 'sage "sage")
    (setq sage-command "$SAGE_ROOT/sage")

where `$INSTALL_DIR` and `$SAGE_ROOT` have been replaced with the respective
paths on you machine. `$INSTALL_DIR` refers to the directory of the sage-mode
Emacs files: these are usually located in

      $SAGE_ROOT/local/share/emacs
   
Furthermore, if you would like typesetting of Sage results, consider also the following lines:

    ;; If you want sage-view to typeset all your output and have plot()
    ;; commands inline, uncomment the following line and configure sage-view:
    (require 'sage-view "sage-view")
    (add-hook 'sage-startup-after-prompt-hook 'sage-view)
    ;; You can use commands like
    ;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-output)
    ;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-plots)
    ;; to enable some combination of features.  Using sage-view requires a
    ;; working LaTeX installation with the preview package.

Also consider running `(customize-group 'sage)` in Emacs to see more options.

### Option 2. Download a pre-compiled spkg at the sage-mode [Bitbucket repository](https://bitbucket.org/gvol/sage-mode)

Once you have downloaded the spkg for the version you want, you install the spkg in Sage by running

    sage -f sage_mode_<version>.spkg

where sage_mode_<version>.spkg refers to the file you have downloaded.

You can then follow the remaining steps of Option 1 to install sage-mode in Emacs.

### Option 3. Download the newest code from the [Bitbucket repository](https://bitbucket.org/gvol/sage-mode) and compile the spkg yourself.

The most maintainable solution is to clone the Bitbucket repository: first, make
sure you have an installation of Mercurial. Then in an appropriate directory, run

     hg clone https://bitbucket.org/gvol/sage-mode

In the resulting `sage-mode` folder, you build the newest spkg simply by running

     ./make-spkg.sh $VERSION

where instead of `$VERSION`, you write a version number. If you write an
existing version number, the corresponding spkg will be built. If you write
something non-existing, the top of tip of the development branch will be built.
If the tip does not have a version number, it might be useful for later
reference to use as `$VERSION$ something which can identify at which point in
the repository, such as the latest version number followed by a few characters
of the latest commit.
TODO: Is this right iandrus?

In any case, this creates a file `sage_mode_<version>.spkg`. You can then follow
the steps in Option 2 as if you had downloaded this spkg from the repository



The inferior sage shell
-----------------------
The primary element of sage-mode is interaction with a Sage shell in a
buffer. Run the function `sage` in Emacs to start it. The new buffer
communicates directly with a Sage shell in the background and behaves very much
like it. You just type and send the command with `<Enter>`:

    sage: 2+2
    4
    sage: x^2 + 1
    x^2 + 1

If you activated `sage-view` in the setup, the last output line should be
properly typeset with LaTeX. This already then beats running the Sage shell in
a usual terminal; however, we are just getting started.

The shell also behaves like an Emacs shell:

- Pressing `M-p` or `C-up` goes through earlier input.
- The history of the shell is earlier in the buffer, and you can move around
  just as in any Emacs buffer. When you're not at the bottom command line, the
  text you enter won't send anything to the shell.
- Pressing `<Enter>` on some line earlier in the buffer runs that line at the
  command line and returns the pointer there.
- Pressing `C-q C-j` inserts a literal newline.

Of course, you also have access to tab-completion and the usual Sage help:

- `<Tab>` at the command line attempts completion of current word. It
  understands all Sage and Python functions currently in scope, and it also
  completes attributes of objects. As usual in Emacs, with multiple suggestions
  they are presented in another window.
- Adding `?` after a name or attribute and then `<Tab>` shows the documentation
  for it in another window. Pressing `<Enter>` instead of `<Tab>` prints the
  documentation in the shell.
- Adding `??` and then `<Enter>` prints the preceding object's source in the shell.
  TODO: /jsrn: This behaves as above when I do it, but when iandrus does it, it
  opens the source file. Why?

You can type `quit` to kill the Sage shell, or you can kill the entire buffer.


Editing your Sage code
----------------------
Most of your code you will want to structure properly and keep, and so will
naturally be edited in separate files and not typed directly into the Sage
shell. When opening a `.sage`-file, sage-mode will be activated, providing you
with syntax highlighting and various interaction with the Sage shell.

If you already have an open Sage shell, the newly opened file should be aware of
it. If not, you can open a new shell with the `sage` function. Now you have
various functions for sending code to the shell:

- `sage-send-buffer` or `C-c C-c` sends the entire buffer's contents to the
   shell.
- `sage-send-defun` or `C-c C-d` sends the `def` that the pointer is currently
   standing in.
- `sage-send-region` or `C-c C-r` sends the currently marked region.
- `sage-attach-this-file` will attach this file to the Sage shell, so that every
   time you execute something in the shell, that file will be reloaded if it has
   changed.

There are a few more such functions; type `C-h m` for a summary. Note that these
functions are not exactly the same as typing in the shell: in particular,
the result of the last line of code is not printed.

Of course, many `.sage`-files can be attached to the same shell, allowing for
quick editing and reloading of your various sources. There is a pitfall here,
though: after modifying a function or class, it is necessary to reload all
functions, classes and objects referring to it, so that the Sage shell is not
using the old definition. Sometimes it is easier or even necessary to restart
the Sage shell in order to achieve this.

If you later restart the Sage shell, you might need to make buffers with
`.sage`-files aware of the new shell. This is most easily done by revisiting the
file: `C-x C-v <Enter>`.



Custom initialisation of Sage
-----------------------------

Perhaps you would like to have Sage initialise with custom code when run from inside Emacs. As usual, you can use `$HOME/.sage/sage.init` for code which should run immediately after Sage starts. To run code only when Sage is run from inside Emacs is then done simply by guarding the code based on the environment variable `$EMACS`:

    import os
    if 'EMACS' in os.environ and os.environ['EMACS']:
        <Custom initialisation code goes here>

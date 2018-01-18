# Emacs Configuration Files

This repository contains an Emacs configuration. No warranty given; use at your own risk.

# Requirements

* Emacs version 24.3 +

# Installation

1. Set your old .emacs.d aside, if it exists: `mv ~/.emacs.d ~/.emacs.d.bak`
2. Set your old .emacs aside, if it exists: `mv ~/.emacs ~/.emacs.bak`
3. Clone the repository to ~/.emacs.d: `git clone <URL TO THIS REPO> ~/.emacs.d`
4. Copy .emacs to home directory: `cp ~/.emacs.d/config/.emacs ~/.emacs`
5. Start emacs - you will see packages automatically installed and process should complete in a few minutes. This installation step will only happen once, after which you are expected to keep your packages up-to-date manually.

# User Specific Settings
If you wish to personalize your configuration, create a directory with your user name under .emacs.d/config/ and add whatever config files you need there. The config files are read in alphabetic order, so I like to use a number-based file naming convention to control the order in which separate files are read. Please see my config folder for an illustrative example (my user name is `mpemer`.
# Keeping Secrets
Emacs is configured to and will read PGP-encrypted files automatically. I have configured my GnuPG software to use my standard keys and I tend to store passwords and secret things (things that I like to keep private) in files ending with the suffix `.pgp` - for example I have the file `~/.emacs.d/config/mpemer/011-org-mode-secrets.el.gpg` that contains some authentication tokens to google calendar. You may want to make use of this feature as well - it comes in very handy, it allows me to publish this git repository as a public repo and the only price is that I have to enter my PGP key password once when I start emacs.

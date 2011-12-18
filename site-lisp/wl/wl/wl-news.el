;;; wl-news.el --- Create notification from NEWS(.ja) for Wanderlust.

;; Copyright (C) 2002 Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;; Copyright (C) 2002 Kenichi OKADA <okada@opaopa.org>

;; Author: Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'elmo)
(require 'wl-vars)
(require 'wl-util)
(require 'wl-address)
(require 'wl-folder)

(defvar wl-news-version-file-name "previous-version")
(defvar wl-news-default-previous-version '(2 0 0))

(defvar wl-news-lang
  (if (and (boundp 'current-language-environment)
	   (string-equal "Japanese"
			 (symbol-value 'current-language-environment)))
      '("ja" "en") '("en" "ja"))
	"The list of languages to show NEWS. (order sensitive)")

(defun wl-news-check ()
  (let* ((updated (not (wl-news-already-current-p))))
    (if updated
	(if (and wl-news-lang
		 (wl-news-check-news
		  (cdr (wl-news-previous-version-load))
		  wl-news-lang)
		 (not (memq 'wl-news wl-hook)))
	    (add-hook 'wl-hook 'wl-news))
      ;; update wl-news-version-file
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load))))
    updated))

;;; -*- news-list -*-

(defconst wl-news-news-alist
  '(("en" ((2 16 0) . "* Changes in 2.16.0 from 2.14.x

** Put spam mark on the message registered as spam.

** Remove spam mark from the message registered as non-spam.

** Added support for ESEARCH feature (RFC4731).

** New option `elmo-imap4-set-seen-flag-explicitly'.

** Optimize refiling.
   Interpret filter, pipe and multi folder and select the most suitable method.

** Speed up bsfilter processing.

** Speed up handling of maildir folder.

** Use IMAP4 non-synchronizing literals (RFC 2088) if it is available.

** Open following thread when you put mark on message in summary buffer.

** Add new command wl-summary-display-raw.

** Use EasyPG (http://www.easypg.org) if it is available.

** A folder type `namazu' is abolished.  New folder type `search' is added instead.

** Fixed against overwriting existing messages in archive folders.
   There was a bug on appending messages.
") ((2 14 1) . "* Changes in 2.14.1 from 2.14.0
  Version 2.14.1 is a bug fix version of 2.14.0.

** Fixed message order of Maildir.

** Icon for the access folder is displayed.
") ((2 14 0) . "* Changes in 2.14.0 from 2.12.2

** New folder type `access' is added.
   In `access' folder, sub-folders of the specified folder can be treated
   as one folder.

** Synchronization speed of the folder is improved.
   The function which calculates list diff is re-wrote and is faster
   than previous implementation, especially in the folders with large
   number of the messages.

** New event handling mechanism is incorporated.

** Improved the disconnected operations for IMAP draft saving.
   There was a bug of message numbering in the disconnected imap draft.
   It is fixed using new event handling mechanism.

** `Shimbun' summary buffers are updated dynamically.
   Some shimbun folder does not have correct information.
   In this version, they are corrected using the message body after retrieval.
   It is implemented with new event handling mechanism.

** Many bug fixes.
") ((2 12 1) . "* Changes in 2.12.1 from 2.12.0
  Version 2.12.1 is a bug fix version of 2.12.0.

** Now Maildir is usable on Windows systems.
   Note that it does not conform to the Maildir standard.

** Fixed the problem of the cache flag inconsistency on the filter folder etc.
   There was a problem that the summary buffer displays cached messages as
   uncached in some folders.

** Fixed the bug that the new flag cannot be changed in some cases.

** Fixed the bug that flag are not taken over correctly from Maildir.
   Only the flag of the first message was taken over in earlier versions.

** Fixed the problem in display module for IMAP messages.
   Now partially fetched messages are displayed correctly.
   If a message included child messages, their headers were not displayed.

** Fixed the problem that %INBOX is not appeared as a subfolder of %INBOX.
   In cyrus-imapd, this problem occurred.

** Now user defined flags are appeared in the completions for search conditions.

** Fixed the problem that a wrong flag folders are created by some flag names.
   If a flag contains a character other than [a-z], the problem occurred.

** Now expansion of the access group \"@/\" works correctly.

** Fixed the problem to cause an error on automatic draft saving.

** Fixed the problem to cause an error on invoking address manager.
   A message which includes a string \"To:\" etc. caused an error.

** Fixed the problem in the flag inheritance function of the filter folder.

** New option `wl-summary-resend-use-cache'.
   You can resend messages using cache in the offline status.

** New option `elmo-network-session-idle-timeout'.
   Network sessions which are not used longer than this value (in seconds)
   are thrown away and new session is created.

** Improved redisplay functions for \"H\" and \"M\" key.
   MIME structure and buffer is reused for redisplay.

** Now attributes for netnews are displayed in the draft preview.
") ((2 12 0) . "* Changes in 2.12.0 from 2.10.1

** The structure of the message database is improved.
   Following setting is to convert legacy msgdb to the new one when you
   select a folder.
   (setq elmo-msgdb-default-type 'standard
         elmo-msgdb-convert-type 'auto)
   (which is initial setting.)

** The temporary mark and corresponding action is now customizable.

   By default, following mark-and-actions are defined.
   mark-and-actions which are defined as before.
    \"o\" refile   (same as before)
    \"O\" copy     (same as before)
    \"d\" dispose  (formerly delete, 'D' mark. Messages are moved to
    		  wl-trash-folder. Its behavior is decided by
		  wl-dispose-folder-alist.)
   New mark-and-actions which are newly introduced.
    \"D\" delete   (remove message immediately)
    \"i\" prefetch (prefetch message)
    \"~\" resend   (resend message)
   Press 'x' to execute actions which corresponds to the mark.
   mark-and-actions can be define by the new variable, 
   'wl-summary-mark-action-list'. See its docstring for more in detail.

** SPAM filter module is added.
   Following spam filter libraries are supported.
   bogofilter
   spamfilter
   bsfilter
   SpamAssassin
   SpamOracle
   Regular Expressions Header Matching

** 'mark folder is renamed to 'flag folder.
   Related to this, original message location is displayed as help-echo on summary
   line in the 'flag folder (you can control the behavior by
   wl-highlight-summary-line-help-echo-alist).

** Now you can put arbitrary user defined flag on message.
   You can specify its flag by \"F\" in the summary mode.

** New marks, 'A' and 'a' are added for answered messages.
   Now answered messages have its own mark in the summary mode.
   'A' is displayed for uncached messages and 'a' is for cached messages.

** New mark,s 'F' and 'f' are added for forwarded messages.
   Now forwarded messages have its own mark in the summary mode.
   'F' is displayed for uncached messages and 'f' is for cached messages.

** New search condition 'Flag' (Status of the message) is added.
   There are flags 'unread', 'important', 'answered',
   'digest' (unread or important) and 'any' (any of the flag).
   For example, following filter folder contains only unread or important
   messages in the %inbox folder.

   /flag:digest/%inbox

** Draft save function is improved.
   Now you can set wl-draft-folder to IMAP folder, Maildir folder, and so on.

** Automatically save draft buffers by using idle-timer.
   You can control behavior by the variable `wl-auto-save-drafts-interval'.

** 'H' key(display all header) and 'M' key(display without MIME analysis)
   are now toggle key.
   Now you can cite messages displayed by 'M'.

** Now you can sort summary lines into descending order.

** Abbreviate too long header extended to lines in message buffer.

** Persistent mark string in summary buffer is changed.
   Default setting indicates cached state by its upper/lower case.

** It displays draft preview on sending confirmation.

** Sending parameters are displayed on draft preview.
   See description of the variable wl-draft-preview-attribute for detail.

** You can run biff with idle-timer by setting wl-biff-use-idle-timer.

** Now wl-draft-kill confirms with yes-or-no-p.

** Summary thread will be divided if its depth is larger than certain amount.
   The limit is controlled by the variable wl-summary-max-thread-depth.

** Emacs multi-tty support is supported.
   (http://lorentey.hu/project/emacs.html)

** New sort spec 'size' is added in the summary mode.
   Now you can sort the summary by message size.

** The variable wl-refile-policy-alist is abolished.

** Batch processing module is added.

** In the multi-folder, status of messages are synchronized with original
   folder.
   For example, unread status of '+inbox' is updated to '*+inbox,+outbox'.

** The function wl-summary-resend-message is abolished.
   you can put mark for resending by wl-summary-resend instead.

** Variables renamed
   wl-delete-folder-alist is renamed to wl-dispose-folder-alist.

** POP3 folder existence check is simplified (by default).
   The default value for elmo-pop3-exists-exactly is changed to nil.

** POP3 response code extension defined in the RFC2449 is supported.
   If a login failure occurred because of user's another POP3 session, 
   entered password is not cleared and used in the future login.

** IMAP4 commands EXPUNGE and CHECK are now send asynchronously.

** Default value of wl-folder-hierarchy-access-folders has been changed.

** Access group \"@/\" of shimbun folders can be used now.

** Show contents of NEWS(.ja) when you start Wanderlust newer than the
   one you used previously.

** Default values of wl-draft-reply-*-list are changed. 
   See samples/en/dot.wl for old values.

** wl-draft-reply-myself-*-list are abolished and integrated into
   wl-draft-reply-*-list.

** You can control initial cursor position for replying draft.
   Set variable wl-draft-reply-default-position appropriately.

** Changed the way to specify configuration of draft buffer window.
   You can choose keep, full or split as values of wl-draft-buffer-style
   and wl-draft-reply-buffer-style.

** Commands to verify/decrypt non-MIME PGP message are added.
   C-c:v, C-c:d in message buffer to verify or decrypt respectively.

** New hooks
   wl-draft-reply-hook
   wl-summary-reply-hook
   wl-draft-forward-hook
   wl-summary-forward-hook
   wl-draft-kill-pre-hook
   wl-summary-resend-hook

** Abolished hook
   wl-reply-hook

** New face

   wl-highlight-summary-disposed-face
   wl-highlight-summary-prefetch-face
   wl-highlight-summary-resend-face
   wl-highlight-summary-answered-face
   wl-highlight-action-argument-face

** Abolished face

   wl-highlight-refile-destination-face
   (renamed to wl-highlight-action-argument-face)
") ((2 10 1) . "* Changes in 2.10.1 from 2.10.0
  Version 2.10.1 is a bug fix version of 2.10.0.

** Fixed the problem that msgdb be destroyed when print-length or
   print-level has Non-nil value.

** wl-summary-pack-number in pipe folder is disabled temporarily
   since it didn't work. Invoke it in destination folder instead.

** Fixed a problem that wl-folder-move-cur-folder doesn't work.

** Fixed a problem that wl-draft-reedit doesn't work properly on Meadow.

** Fixed a problem that wl-summary-pack-number doesn't work on Maildir and
   shimbun folders.

** Fixed a problem that cache file is not protected even if it is marked
   as important.

** Fixed a problem that %# in wl-summary-line-format cannot handle large
   number.

** Fixed a problem to remove password even if SMTP AUTH failed at non-auth
   phase.

** Default value of wl-message-buffer-prefetch-folder-type-list,
   wl-message-buffer-prefetch-idle-time, and
   wl-message-buffer-prefetch-depth are changed.

** Fixed to compile on XEmacs without mule feature.
") ((2 10 0) . "* Changes in 2.10.0 from 2.8.1

** You can alter the format of summary lines.
   Specify format by wl-summary-line-format. If you want to change ones
   according to folder names, use wl-folder-summary-line-format-alist.

** Save format for the draft folder has been changed. Messages are encoded
   before saved by wl-draft-save.

** elmo-split is newly established. It provides a way to split messages
   according to some rule a la procmail.

** Buffer prefetch works fine now. Messages of the number specified by
   wl-message-buffer-prefetch-depth are loaded into buffer in advance.

** elmo-dop-queue-flush flushes queue that concerns plugged folder.

** Starting Wanderlust on the new frame is possible now. Set as
   (autoload 'wl-other-frame \"wl\" \"Wanderlust on new frame.\" t)

** In Folder mode, you can go into virtual folder which consists of messages
   with some specified condition (wl-folder-virtual). It is binded to \"V\".

** In Folder mode, you can search folders containing messages with some
   specified condition (wl-folder-pick). It is binded to \"?\".

** Now you can rename access group folders.

** You can specify ON/OFF of thread view for newly created summary.
   Set wl-summary-default-view, wl-summary-default-view-alist.

** Temporary marks are kept when you exit from sticky summary by q or g. 

** Key bindings concerning the sticky summary have been changed.
   By C-u g, the sticky summary is destroyed as well as C-u q. In summary or
   folder mode, G opens the sticky summary.

** You can go round summary buffers by C-cC-n and C-cC-p.

** Members of the list wl-folder-hierarchy-access-folders is now some REGEXP
   for access group names instead of exact group names.

** In header part of the draft buffer C-a brings cursor to the beginning of
   the line or the beginning of the header body.

** You can send encapsulated blind carbon copies. Its default field name is
   \"Ecc:\".

** C-c C-y (Draft) can cite region of the message.
   It affects if transient-mark-mode (Emacs) or zmacs-regions (XEmacs) is
   Non-nil and the region is active.

** You can delete a part from multipart message.
   It is binded as \"D\" in message buffer.

** You can easily configure server settings to post news article.
   Set wl-nntp-posting-config-alist appropriately. See Info for an example.

** You can specify some function in wl-draft-reply-with-argument-list etc.
   for setting the recipients in draft by the return value of it.

** The interface of the function wl-draft has been changed.
   The initial set of headers are handed as an association list.

** The uses of wl-generate-mailer-string-function has been changed.
   Specify a function which returns some string to appear in User-Agent header.

** The Reference Card (doc/wl-refcard.tex) describes important key bindings.

** Many bug fixes.
") ((2 8 0) . "* Changes in 2.8.0 from 2.6.1

** Nemacs, Mule 2.3 based on Emacs 19.28 are not supported any longer.

** Wanderlust might not work with FLIM 1.14.2 and older.
   It is recommended to use FLIM 1.14.3 or newer and associated SEMI.

** Now available `make check' environment test for user.

** If you set obsolete variables (e.g. renamed ones) in .wl etc, Wanderlust
   shows warning messages and urge you to change settings.
   Change your settings according to the messages, please.
   If you want to suppress warnings, set elmo-obsolete-variable-show-warnings
   to nil.

** Added new internal folders: 'sendlog folder

** Added new type of folders: shimbun folder

   Format: '@' 'virtual server name' '.' 'group name'

** Added new type of folders: namazu folder

   Format:  '[' 'search condition' ']' [ 'absolute path of namazu index' ]

** With pipe folder, now you can preserve messages on the server.
   At the next time you access it, only new messages will be copied.

   Format:  '|' 'source folder' '|:' 'destination folder'

** Address manager is now available (start by C-c C-a).
   You can edit address book and import recipients to draft from it.

** ACAP (RFC2244) is supported(experimental).

** Now you can preserve IMAP4 message by part as a cache.
   If you skipped enormous part, you can read other than skipped part when
   you are off line.

** Wanderlust also creates message view through prefetching.
   Displaying of prefetched messages speeded up because of this.

** Truncation of lines in message buffer or draft buffer is now controllable.
   Non-nil value of wl-message-truncate-lines or wl-draft-truncate-lines
   means truncating long lines at window width.

** Bitmap image for opening demo is removed from wl-demo.elc and now loaded
   from wl-icon-directory.
   Special logo is displayed through the Christmas season :)

** Overall elmo module is rewritten.

** Variables depending on elmo backends are renamed to \"elmo-backend-*\".
   e.g. elmo-default-imap4-server is renamed to elmo-imap4-default-server.

** Variables named xxx-func are renamed to xxx-function.

** X-Face utility 1.3.6.12 or older is not supported any longer.
   Please install X-Face utility 1.3.6.13 or later, if necessary.

** Wanderlust distinguishes stream-type on plugged mode. They are treated as
   different entries.

** msgdb path for archive and multi folders are changed.
   No problem for running wanderlust even if you do not deal with them.
   But if you don't want to leave useless data on the disk, delete under
   .elmo/multi and .elmo/archive in advance.

** Variables named xxx-dir are renamed to xxx-directory.
   e.g. wl-icon-dir is renamed to wl-icon-directory.
   Take attention if you set for display of startup logo, etc.

** elmo-cache-dirname is abolished and elmo-cache-directory is newly created.
   You can put cache directory to another place by setting
   elmo-cache-directory.

** Default value of elmo-enable-disconnected-operation is now `t'.
   When the relevant messages are cached, you can do some operations
   even in the off-line state.

** Now messages with \"$\" mark is not remained in the summary buffer when
   the actual message itself is deleted.
   Please visit the 'mark folder to review the messages with the \"$\" mark.
") ((2 6 1) . "* Changes in 2.6.1 from 2.6.0
  Version 2.6.1 is basically a bug fix version of 2.6.0.

** Fixed a problem that Emacs 21 causes `Recursive load...' error.

** Fixed a problem that thread character is broken in XEmacs 21.1.

** Fixed a problem that in IMAP4 folder, progress bar is remained in XEmacs .

** Fixed a problem that searching is failed for the header fields that
   begins with X-.

** Some other fixes.
") ((2 6 0) . "* Changes in 2.6.0 from 2.4.1

** FLIM 1.13.x is not supported any longer.
   Please install FLIM 1.14.1 or later.

** Now folder and summary buffer can be opened in a separate frame.
   If `wl-folder-use-frame' is set as t, `M-x wl' creates a new frame
   for folder mode. If `wl-summary-use-frame' is set as t, new frames
   are created for each summary window.

** Cursor moving speed ('N' or 'P' in summary) is greatly improved.

** Folder checking speed for filter folder of localdir
   folder using `last' or `first' (Ex. /last:100/+inbox) is improved.

** Retrieval progress of each message is displayed in POP and IMAP folder.

** Coloring of summary buffer is processed on demand (only on Emacs).
   If `wl-summary-lazy-highlight' is non-nil, 
   only visible portion of the buffer is colored.

** Customizable biff notify.
   New hook `wl-biff-notify-hook' and `wl-biff-unnotify-hook' is
   now available.
   e.g. (add-hook wl-biff-notify-hook 'ding)

** Many bug fixes.
") ((2 4 1) . "* Changes in 2.4.1 from 2.4.0
  Version 2.4.1 is basically a bug fix version of 2.4.0.

** Wanderlust 2.4.1 now works on FLIM 1.14.x. 

** Fixed a problem that POP connection remains after POP before SMTP.

** The specification of IMAP4 authentication method for clear password
   is changed.

In 2.4.0, To use clear password authentication method in IMAP4
\(Logging in with LOGIN command), you have to set the variable
`elmo-default-imap4-authenticate-type' as 'plain (or nil).
But in 2.4.1, it is changed to 'clear (or nil).
Example:
\(setq elmo-default-imap4-authenticate-type 'plain)
should be changed to
\(setq elmo-default-imap4-authenticate-type 'clear)
") ((2 4 0) . "* Changes in 2.4.0 from 1.1.1

** Version Number
The version numbering convention for Wanderlust is changed. 

In earlier versions, 1.x were stable version and from 2.0.x to 2.2.x
were beta version. But since version 2.3.0, the second (minor) version
number implies the stability of the Wanderlust. Even minor number
corresponds to a stable version, and an odd minor number corresponds
to a development version. This version numbering is based on the
widespread convention of open source development.

On the open CVS server cvs.m17n.org, main trunk contains the current
beta (newest experimental) version, and branches contain the stable
version.  (If the version is 2.4.x, the branch name is wl-2_4)

** Install

*** FLIM 1.12 is not supported anymore.
See the file INSTALL for details.

*** APEL 10.2 or later is required.
tm-8 users should check the version of APEL (tm-8.8 contains old APEL).

** New feature

*** LDAP support
Complete e-mail address in draft by searching LDAP server.
If the variable wl-use-ldap is non-nil, LDAP feature is enabled
\(Initial setting is nil).

*** UIDL support in POP3 folder
POP3 folder now saves the status of summary and it improves summary
update speed. If the variable elmo-pop3-use-uidl is non-nil, UIDL is
used (Initial setting is t).

*** Emacs 21 support
Wanderlust has started on supporting Standard Emacs 21.
Toolbars and icon images can be shown in almost Wanderlust
frames like XEmacs.

*** biff feature
Server mailbox is checked periodically.
If new mail is arrived, Wanderlust changes the biff (icon) on the modeline
and updates folder mode content.

*** expire-hide 
Now expire mechanism has new feature `hide', it does not remove
messages actually from folder but hides messages from summary. It
improves processing speed for large folders.

*** Message thread restoring feature
Automatic correction of broken threads by subject matching is now available.
Thread modification by hand (M-w (copy) and C-y (paste) in summary mode)
is also available.

*** Password expiration timer
Password cache expires after elmo-passwd-life-time is passed.
\(nil means no expiration. Initial setting is nil)

*** killed-list
Deleted messages in the NNTP folder are saved to `killed-list'.  The
messages in the killed-list are treated as if it were not exist on the
server. Non-nil value for elmo-use-killed-list enables this feature
\(Initial setting is t). By this feature, NNTP pipe folder works correctly.

*** Maildir pack is now available
M-x wl-summary-pack-number in the summary mode of Maildir folder
re-numbers the messages.

** Searching

*** Complex condition can be specified for filter folder
AND condition, OR condition, NOT condition, and their combination can be
 specified. Syntax of the condition part is changed. See Info for details.

Caution for those who upgrade from 1.1.1:
By this change, saving directory for the msgdb of filter folder is altered.
Former msgdbs are not needed anymore. It does not cause any problem but
if you don't want to keep useless disk, you should remove files
under the directory '.elmo/filter/' beforehand.

*** Searching of the NNTP folder is available
Now you can make NNTP filter folder.
\(If only your NNTP server responds to XHDR command.)

*** Pick, Virtual in summary mode now accepts complex condition.
You can set AND condition and OR condition by typing
'AND' or 'OR' instead of field name.

** Session, Authentication

*** elmo-default-*-authenticate-type only accepts symbol(used be a string)
Example:
\(setq elmo-default-imap4-authenticate-type \"cram-md5\")
should be changed to
\(setq elmo-default-imap4-authenticate-type 'cram-md5)

*** stream-type can be defined.
You can define stream type by
elmo-network-{imap4-,pop3-,nntp-,}stream-type-alist.
Some SSL related variables are abolished(renamed).
You can access to the networked folders (IMAP4, NNTP, POP3) via SOCKS
if you specify the folder name end with \"!socks\".

** Draft

*** group-list is now available
You can specify address like 'Group: foo@gohome.org, bar@gohome.org;'.
If wl-draft-remove-group-list-contents is non-nil, the contents of 
group-list is removed before sending.

*** The draft preview displays recipient addresses on minibuffer 
You can confirm the group-list recipients by this.

*** Initial setting considers Reply-To:.
Default setting of wl-draft-reply-without-argument-list considers Reply-To: 
field (Set to To: field).

*** Replying rules for the messages sent from yourself.
You can define replying rules for the messages sent from yourself by
setting wl-draft-reply-myself-with-argument-list and
wl-draft-reply-myself-without-argument-list.

*** Full name is used in the reply address.
If wl-draft-reply-use-address-with-full-name is non-nil, then full
name is inserted in with e-mail addresses on the replied message
\(Initial setting is t).

*** In-Reply-To: format is changed.
In-Reply-To: format is changed to simple one. It is based on 
draft-ietf-drums-msg-fmt-09.txt.

** misc

*** Message thread processing is improved.

*** Renamed variables
wl-refile-guess-func-list => wl-refile-guess-functions
wl-summary-temp-above => wl-summary-target-above

*** You can set function to wl-fcc.
You can change fcc folder name dynamically. For example, change folder name
by month.

*** elmo-search-mime-charset is abolished.
Charset is guessed from the string you typed.

*** Useless headers are removed when you forward the message.
You can specify removed headers by wl-ignored-forwarded-headers.

*** wl-highlight-group-folder-by-numbers is abolished.
It is renamed to wl-highlight-folder-by-numbers and has following meaning.
  `t'   : Whole line is colored by message number.
  `nil' : Whole line is colored by folder status.
   Number (ex. `1') : Line is colored by message number and folder status.

*** Header visibility control is changed.
Header visibility is controlled by Wanderlust (was controlled by SEMI).
You can change header visibility by wl-message-ignored-field-list and 
wl-message-visible-field-list.

*** DEMO is changed.
Less colors are used by DEMO pixmap.
Emacsen on character based terminal also display suitable DEMO.
") ((1 1 1) . "* Changes in 1.1.1 from 1.1.0
  Version 1.1.1 is a bug fix version of 1.1.0 with minor user-visible changes.

** Development on the CVS server is started.

** Flush operation and sending queues if Wanderlust is  started
   in plugged status.

** Directory structure is changed.

*** 00README, 00README.ja is renamed to README, README.ja.

*** All wl-* files are moved to the directory 'wl'.

** Syntax of wl-refile-rule-alist is extended (compatible with older one).

** progress gauge
Progress gauge is displayed while processing in the Emacsen with
progress gauge feature.
") ((1 1 0) . "* Changes in 1.1.0 from 1.0.3

** Install

*** tm7 is not supported anymore.
see the file INSTALL for details.

*** WL_PREFIX and ELMO_PREFIX default as \"wl\"
\(defvar WL_PREFIX \"wl\")
\(defvar ELMO_PREFIX \"wl\")

e.g. install directory is
  1.0.3  /usr/local/share/emacs/site-lisp/
  1.1.0  /usr/local/share/emacs/site-lisp/wl/

*** Change default macro in Makefile.
EMACS   = emacs
XEMACS  = xemacs
use $(XEMACS), `package' and `install-package' target.

*** Install not only *.elc, but also *.el.

*** English document (wl.texi).

** New feature

*** Modified UTF7 support.
Now international mailbox name can be used in IMAP4 in the Emacsen
with unicode feature.

*** Scoring support.

*** New plugged system.

*** IMAP4 support became more generic.
Many IMAP4 servers are supported.

*** New authentication type
  IMAP4: CRAM-MD5, DIGEST-MD5, STARTTLS
  POP3:  CRAM-MD5, DIGEST-MD5, SCRAM-MD5, STARTTLS
  NNTP:  STARTTLS
  SMTP:  STARTTLS

*** New folder type
  |      Pipe Folder     Incorporate message.
  .      Maildir Folder  Now Maildir is one of the folder type.
  'cache Cache Folder    View internal cache.

*** Message buffer cache
Next message is prefetched while idle time.

*** Sticky summary is enhanced.
Now message buffer is also sticky.
You can specify always-sticky summary.

** misc

*** Eliminated wl-draft-prepared-config-alist
unified with wl-draft-config-alist.

*** POP-before-SMTP variables are re-arranged.

*** Ask non-existing folder.
 When FCC: contains new folder.
 When auto-refile specified new folder.

*** Change fetch threshold and confirm settings.
wl-prefetch-confirm-threshold, wl-cache-fetch-threshold.

*** Can use petname for completion.

*** Change Message-ID generator.

*** wl-demo.el support bitmap-mule.

*** Allow function type `smtp-server' value.

*** Make sendlog when `wl-draft-sendlog' is non-nil.

*** `wl-summary-incorporate-marks'

*** Reserve prefetching while off-line status.

*** Draft use new frame when `wl-draft-use-frame' is non-nil.

*** New variable `wl-user-mail-address-list' .

*** New variable `wl-local-domain' for set FQDN.

*** Server side unread status is used in IMAP4 folder.

*** Change defaults
  wl-mime-charset         iso-2022-jp  =>  x-ctext
  wl-summary-move-order   'new  =>  'unread
  wl-tmp-dir              TMPDIR  =>  ~/tmp/

*** New hooks
  wl-draft-send-hook
  wl-draft-reedit-hook
  wl-mime-edit-preview-message-hook
  wl-folder-suspend-hook
  wl-summary-toggle-disp-folder-message-resumed-hook
  wl-summary-line-inserted-hook
  wl-thread-update-children-number-hook
  mmelmo-header-inserted-hook
  mmelmo-entity-content-inserted-hook

*** New function
  wl-save
  wl-summary-write
  wl-summary-supersedes-message
  wl-fldmgr-delete
  wl-refile-guess-by-msgid
  wl-address-user-mail-address-p
  wl-summary-jump-to-msg-by-message-id-via-nntp
  wl-summary-temp-mark-pick
")) ("ja" ((2 16 0) . "* 2.14.x $(B$+$i(B 2.16.0 $(B$X$NJQ99E@(B

** spam $(B$H$7$FEPO?$7$?%a%C%;!<%8$K(B spam $(B%^!<%/$rIU$1$^$9!#(B

** non-spam $(B$H$7$FEPO?$7$?%a%C%;!<%8$+$i(B spam $(B%^!<%/$r<h$j=|$-$^$9!#(B

** ESEARCH (RFC4731) $(B$,%5%]!<%H$5$l$^$7$?!#(B

** $(B?75,%*%W%7%g%s(B elmo-imap4-set-seen-flag-explicitly $(B$,DI2C$5$l$^$7$?!#(B

** pipe $(B$G$N%3%T!<$G?75,%a%C%;!<%8$,<h$j9~$a$J$/$J$k%P%0$,=$@5$5$l$^$7$?!#(B

** $(B%5%^%j$N%=!<%H=g$KJ#?t$N>r7o$r;XDj=PMh$k$h$&$K$J$j$^$7$?!#(B
   \",\" $(B6h@Z$j$G>r7o$rJB$Y$F2<$5$$!#(B

** $(B%M%C%H%o!<%/%U%)%k%@L>$N2r@O$r87L)$K9T$&$h$&$K$J$j$^$7$?!#(B

** $(B%j%U%!%$%k=hM}$N:GE,2=(B
   filter, pipe, multi $(B%U%)%k%@$r2r<a$7!":GE,$JJ}K!$rA*Br$7$^$9!#(B

** bsfilter $(B=hM}$N9bB.2=(B

** maildir $(B%U%)%k%@$N=hM}$N9bB.2=(B

** IMAP4 $(BHsF14|%j%F%i%k(B(RFC 2088)$(B$KBP1~(B

** $(B%5%^%j%P%C%U%!$G%a%C%;!<%8$K%^!<%/$r$D$1$?;~$K%9%l%C%I$r3+$-$^$9!#(B

** wl-summary-display-raw $(B$,DI2C$5$l$^$7$?!#(B

** EasyPG (http://www.easypg.org) $(B$,MxMQ2DG=$G$"$l$P;H$$$^$9!#(B

** namazu $(B%U%)%k%@$O5!G=$r3HD%$7$?(B search $(B%U%)%k%@$KCV$-49$($i$l$^$7$?!#(B
   namazu $(B0J30$N30It%W%m%0%i%`$r;H$C$?8!:w$,=PMh$k$h$&$K$J$j$^$7$?!#%G(B
   $(B%U%)%k%H$G$O!"(Bgrep $(B$G$N8!:w$,%5%]!<%H$5$l$F$$$^$9!#(B

** archive $(B%U%)%k%@$N4{B8$N%a%C%;!<%8$r>e=q$-$7$F$7$^$&LdBj$,=$@5$5$l$^$7$?!#(B
   $(B%a%C%;!<%8$rDI2C$9$k=hM}$K%P%0$,B8:_$7$F$$$^$7$?!#(B
") ((2 14 1) . "* 2.14.0 $(B$+$i(B 2.14.1 $(B$X$NJQ99E@(B
  2.14.1 $(B$O!"(B2.14.0 $(B$N%P%0=$@5HG$G$9!#(B

** Maildir $(B$GI=<(=g=x$,@5$7$/$J$$%P%0$,=$@5$5$l$^$7$?!#(B

** $(B%"%/%;%9%U%)%k%@$N%"%$%3%s$,I=<($5$l$^$9!#(B
") ((2 14 0) . "* 2.12.2 $(B$+$i(B 2.14.0 $(B$X$NJQ99E@(B

** $(B?7$7$$%U%)%k%@7?(B access $(B%U%)%k%@$,DI2C$5$l$^$7$?!#(B
   $(B;XDj$5$l$?%U%)%k%@$NG[2<$N%5%V%U%)%k%@$r2>A[E*$K0l$D$N%U%)%k%@$H$7$F07$((B
   $(B$k$h$&$K$9$k%U%)%k%@$G$9!#(B

** $(B%U%)%k%@$N%"%C%W%G!<%H$,9bB.2=$5$l$^$7$?!#(B
   $(B%j%9%H$N:9J,$r7W;;$9$k4X?t$,=q$-D>$5$l!"FC$KB?$/$N%a%C%;!<%8$r4^$`%U%)(B
   $(B%k%@$G$NF0:n$,9bB.$K$J$j$^$7$?!#(B

** $(B?7$7$$%$%Y%s%H%O%s%I%j%s%05!G=$,DI2C$5$l$^$7$?!#(B

** $(B%I%i%U%H%U%)%k%@$K(B IMAP $(B%U%)%k%@$r;XDj$7$F$$$k>l9g$NIT6q9g$,2~A1$5$l$^$7$?!#(B
   $(B%*%U%i%$%s>uBV$G%I%i%U%H$rJ]B8$9$k$H$-$N5sF0$K%P%0$,$"$j$^$7$?$,!"?7$7$$(B
   $(B%$%Y%s%H%O%s%I%j%s%05!G=$rMQ$$$F=$@5$5$l$^$7$?!#(B

** `Shimbun' $(B$N%5%^%j$,F0E*$K99?7$5$l$^$9!#(B
   $(B$$$/$D$+$N(B shimbun $(B%U%)%k%@$O!"%5%^%j$N>pJs$,@5$7$/$"$j$^$;$s!#(B
   $(B$3$N%P!<%8%g%s$+$i!"%a%C%;!<%8$r<h$j$h$;$?$H$-$N>pJs$rMQ$$$F%5%^%j$,(B
   $(B<+F0E*$K=$@5$5$l$k$h$&$K$J$j$^$7$?!#$3$N<BAu$K$O!"?7$7$$%$%Y%s%H%O%s%I(B
   $(B%j%s%05!G=$,MQ$$$i$l$F$$$^$9!#(B

** $(B$=$NB>B?$/$N%P%0=$@5!#(B
") ((2 12 1) . "* 2.12.0 $(B$+$i(B 2.12.1 $(B$X$NJQ99E@(B
  2.12.1 $(B$O!"(B2.12.0 $(B$N%P%0=$@5HG$G$9!#(B

** Windows $(B$G(B Maildir $(B$,;H$($k$h$&$K$J$j$^$7$?!#(B
   $(B$?$@$7!"5,3J$KB'$C$F$$$^$;$s$N$G!"(BUNIX $(B>e$N(B Maildir $(B$H8_49@-$,$"$j$^$;$s!#(B

** $(B%U%#%k%?%U%)%k%@Ey$G$N%-%c%C%7%e%^!<%/IT@09g$NIT6q9g$,2r>C$5$l$^$7$?!#(B
   $(B%-%c%C%7%e$5$l$F$b%-%c%C%7%e$5$l$F$$$J$$I=<($H$J$k>l9g$,$"$j$^$7$?$,(B
   $(B=$@5$5$l$^$7$?!#(B

** $(B%U%i%0$,?75,$N$^$^JQ99$5$l$J$/$J$k>l9g$,$"$k%P%0$N=$@5!#(B

** Maildir $(B$GJ#?t%a%C%;!<%80\F0;~!"%U%i%0$,@5$7$/0z$-7Q$,$l$J$$%P%0$N=$@5!#(B
   $(B:G=i$N%a%C%;!<%8$7$+%U%i%0$,0z$-7Q$,$l$^$;$s$G$7$?$,!"=$@5$5$l$^$7$?!#(B

** $(BF~$l;R$K$J$C$?%a%C%;!<%8$N(B IMAP $(B$K$h$kI=<(;~$NIT6q9g$,2r>C$5$l$^$7$?!#(B
   $(BF~$l;R$K$J$C$?%a%C%;!<%8$r%Q!<%H%U%'%C%A$7$?>l9g!"%X%C%@$,I=<($5$l$J$$(B
   $(B>l9g$,$"$j$^$7$?$,!"@5$7$/I=<($5$l$k$h$&$K$J$j$^$7$?!#(B

** %INBOX $(B$N%5%V%U%)%k%@$K(B %INBOX $(B<+BN$,4^$^$l$J$$>l9g$,$"$kLdBj$KBP=h$7$^$7$?!#(B
   cyrus-imapd $(B$G!"Ev3:$NLdBj$,=P$F$$$^$7$?$,=$@5$5$l$^$7$?!#(B

** $(B%U%i%0$,!"8!:w;~Ey$NJd408uJd$H$7$F8=$l$k$h$&$K$J$j$^$7$?!#(B

** Folder mode $(B$G%"%/%;%9%0%k!<%W(B \"@/\" $(B$NE83+$,@5$7$/F0:n$7$^$9!#(B

** $(B%U%i%0L>$K$h$C$F$O4V0c$C$?%U%i%0%U%)%k%@$,:n@.$5$l$kLdBj$,=$@5$5$l$^$7$?!#(B
   $(B%U%i%0$K(B [a-z]$(B0J30$NJ8;z$r;H$C$?$H$-$NLdBj$KBP=h$7$^$7$?!#(B

** $(B%I%i%U%H$N<+F0J]B8;~$K%(%i!<$K$J$C$F$7$^$&LdBj$,=$@5$5$l$^$7$?!#(B

** $(B%"%I%l%9%^%M!<%8%c$r5/F0$7$?:]$K%(%i!<$K$J$kLdBj$,=$@5$5$l$^$7$?!#(B
   $(BK\J8$K(B To: $(BEy$NJ8;zNs$,$"$k$H%(%i!<$,H/@8$7$F$$$^$7$?$,!"=$@5$5$l$^$7$?!#(B

** $(B%U%#%k%?%U%)%k%@$+$i$N%3%T!<Ey$G%U%i%0$,J]B8$5$l$J$$LdBj$,=$@5$5$l$^$7$?!#(B

** $(B?75,%*%W%7%g%s(B wl-summary-resend-use-cache $(B$,DI2C$5$l$^$7$?!#(B
   $(B%*%U%i%$%s>uBV$G$b%-%c%C%7%e$rMQ$$$?:FAw(B(resend)$(B$,$G$-$^$9!#(B

** $(B?75,%*%W%7%g%s(B elmo-network-session-idle-timeout $(B$,DI2C$5$l$^$7$?!#(B
   $(B;XDj$7$?;~4V0J>e%"%$%I%k>uBV$H$J$C$?%;%C%7%g%s$r:FMxMQ$7$^$;$s!#(B

** 'H' $(B$d(B 'M' $(B$G$N%a%C%;!<%8:FI=<($,8zN(2=$5$l$^$7$?!#(B
   $(B:FI=<(;~$K(B MIME $(B$N9=B$!"%P%C%U%!$r:FMxMQ$9$k$h$&$K$J$j$^$7$?!#(B

** $(B%I%i%U%H$N%W%l%S%e!<;~$KI=<($5$l$kB0@-I=<($,(B netnews $(B$KBP1~$7$^$7$?!#(B
") ((2 12 0) . "* 2.10.1 $(B$+$i(B 2.12.0 $(B$X$NJQ99E@(B

** $(B%a%C%;!<%8%G!<%?%Y!<%9$N9=B$$,2~A1$5$l$^$7$?!#(B
   $(B0J2<$N@_Dj$r$9$l$P!"%U%)%k%@A*Br;~$K<+F0E*$K5lMh$N%?%$%W$N(B msgdb $(B$r(B
   $(B?7$7$$%?%$%W$N$b$N$KJQ49$7$^$9!#(B
   (setq elmo-msgdb-default-type 'standard
         elmo-msgdb-convert-type 'auto)
   $(B=i4|CM$O!">e5-$NDL$j$H$J$C$F$$$^$9!#(B

** $(B0l;~%^!<%/$H!"$=$l$KBP$9$k%"%/%7%g%s$r<+M3$KDj5A$G$-$k$h$&$K$J$j$^$7$?!#(B

   $(B%G%U%)%k%H$G$O0J2<$N%^!<%/$H%"%/%7%g%s$rDj5A$7$F$$$^$9!#(B
   $(B=>Mh$+$i0z$-7Q$,$l$?%^!<%/$H%"%/%7%g%s(B
    \"o\" refile   ($(B=>Mh$N%j%U%!%$%k$HF1$8(B)
    \"O\" copy     ($(B=>Mh$N%3%T!<$HF1$8(B)
    \"d\" dispose  ($(B5l(B delete, D $(B%^!<%/!#(Bwl-trash-folder $(B$K0\F0!#(B
    		  wl-dispose-folder-alist $(B$NCM$K$h$j5sF0$,7h$^$k!#(B)
   $(B?7$?$KDI2C$5$l$?%^!<%/$H%"%/%7%g%s(B
    \"D\" delete   ($(B$$$-$J$j>C5n(B)
    \"i\" prefetch ($(B%W%j%U%'%C%A(B)
    \"~\" resend   ($(B:FAw(B)
   $(B%5%^%j$G(B x $(B%-!<$r2!$9$H%^!<%/$KBP1~$7$?%"%/%7%g%s$,$9$Y$F<B9T$5$l$^$9!#(B
   $(B%^!<%/$H%"%/%7%g%s$O!"?75,JQ?t(B wl-summary-mark-action-list $(B$K$h$C$FDj5A(B
   $(B$G$-$^$9!#>\$7$/$OF1JQ?t$N(B docstring $(B$r;2>H$7$F$/$@$5$$!#(B

** $(B%9%Q%`%U%#%k%?%b%8%e!<%k$,?7$?$KDI2C$5$l$^$7$?!#(B
   $(B0J2<$N%9%Q%`%U%#%k%?$KBP1~$7$F$$$^$9!#(B
   bogofilter
   spamfilter
   bsfilter
   SpamAssassin
   SpamOracle
   $(B@55,I=8=$K$h$k%X%C%@8!::(B

** 'mark $(B%U%)%k%@$O2~L>$5$l!"(B'flag $(B%U%)%k%@$K$J$j$^$7$?!#(B
   $(B$3$l$K4XO"$7$F!"(B'flag $(B%U%)%k%@$N%5%^%j$G85%a%C%;!<%8$,$I$3$K$"$k$+$r(B
   help-echo $(B$H$7$FI=<($9$k$h$&$K$J$j$^$7$?(B($(B$3$l$N$U$k$^$$$O(B
   wl-highlight-summary-line-help-echo-alist $(B$G@)8f$G$-$^$9(B)$(B!#(B

** $(B%a%C%;!<%8$KBP$7$FG$0U$N%f!<%6Dj5A$N%U%i%0$rIU$1$i$l$k$h$&$K$J$j$^$7$?!#(B
   $(B%5%^%j$K$*$$$F(B \"F\" $(B$G%U%i%0$N;XDj$,$G$-$^$9!#(B

** $(BJV?.:Q$_%^!<%/(B A,a $(B$,DI2C$5$l$^$7$?!#(B
   $(B%5%^%j$K$*$$$F!"JV?.$7$?%a%C%;!<%8$K(B A $(B%^!<%/(B($(B%-%c%C%7%e$J$7$N>l9g(B)
   $(B$b$7$/$O(B a $(B%^!<%/(B($(B%-%c%C%7%e$"$j$N>l9g(B) $(B$,I=<($5$l$^$9!#(B

** $(BE>Aw:Q$_%^!<%/(B F,f $(B$,DI2C$5$l$^$7$?!#(B
   $(B%5%^%j$K$*$$$F!"E>Aw$7$?%a%C%;!<%8$K(B F $(B%^!<%/(B($(B%-%c%C%7%e$J$7$N>l9g(B)
   $(B$b$7$/$O(B f $(B%^!<%/(B($(B%-%c%C%7%e$"$j$N>l9g(B) $(B$,I=<($5$l$^$9!#(B

** $(B%U%)%k%@$N8!:w>r7o$K!"(B'$(B%U%i%0(B' ($(B%a%C%;!<%8$N>uBV(B) $(B$,DI2C$5$l$^$7$?!#(B
   $(B%U%i%0$K$O!"(Bunread($(BL$FI(B), important($(B=EMW(B), answered($(BJV?.:Q$_(B),
   digest ($(BL$FI$^$?$O=EMW(B), any ($(BL$FI$^$?$O=EMW$^$?$OJV?.:Q$_(B)$(B$,$"$j$^$9!#(B
   $(BNc$($P!"<!$N%U%#%k%?%U%)%k%@$O!"(B%inbox $(B$N$&$A!"L$FI$^$?$O=EMW$J(B
   $(B%a%C%;!<%8$N$_$,=P8=$7$^$9!#(B
   /flag:digest/%inbox

** $(B%I%i%U%H$NJ]B85!G=$,2~A1$5$l$^$7$?!#(B
   IMAP $(B%U%)%k%@$d!"(BMaildir $(B%U%)%k%@$r(B wl-draft-folder $(B$K;XDj$G$-$k$h$&$K(B
   $(B$J$j$^$7$?!#(B

** idle-timer $(B$rMxMQ$7$F%I%i%U%H%P%C%U%!$N<+F0J]B8$r<B9T$7$^$9!#(B
   $(BJQ?t(B `wl-auto-save-drafts-interval' $(B$G5sF0$rJQ$($i$l$^$9!#(B

** 'H' $(B%-!<(B($(B%X%C%@A4I=<((B) $(B$*$h$S(B 'M' $(B%-!<(B(MIME$(B$J$7I=<((B)$(B$,%H%0%k$K$J$j$^$7$?!#(B
   $(B$^$?!"(B'M' $(B$GI=<($7$?%a%C%;!<%8$r0zMQ$G$-$k$h$&$K$J$j$^$7$?!#(B

** non-MIME PGP $(B%a%C%;!<%8$N8!>Z!"I|9f2=$N$?$a$N%3%^%s%I$,DI2C$5$l$^$7$?!#(B
   $(B%a%C%;!<%8%P%C%U%!$K$*$$$F(B C-c:v, C-c:d $(B$G$=$l$>$l8!>Z!"I|9f2=$7$^$9!#(B

** $(B%5%^%j$rJB$YBX$(>r7o$N5U=g$G%=!<%H$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(B%a%C%;!<%8%P%C%U%!$GJ#?t9T$K$o$?$kD9$$%X%C%@$r>JN,I=<($7$^$9!#(B

** $(B%5%^%j%P%C%U%!$N1JB3%^!<%/$NJ8;zNs$,JQ99$5$l$^$7$?!#(B
   $(B%G%U%)%k%H$N@_Dj$G$O%-%c%C%7%e$NM-L5$rBgJ8;z>.J8;z$G<($7$^$9!#(B

** $(BAw?.3NG'$N:]$K%I%i%U%H$N%W%l%S%e!<$rI=<($9$k$h$&$K$J$j$^$7$?!#(B

** $(B%I%i%U%H$N%W%l%S%e!<$N:]$KAw?.%Q%i%a!<%?$rI=<($9$k$h$&$K$J$j$^$7$?!#(B
   $(B>\$7$/$OJQ?t(B wl-draft-preview-attributes $(B$N@bL@$r8+$F2<$5$$!#(B

** wl-biff-use-idle-timer $(B$r@_Dj$9$k$H(B biff $(B$,(B idle-timer $(B$GAv$j$^$9!#(B

** wl-draft-kill $(B$O(B yes-or-no-p $(B$G3NG'$r5a$a$k$h$&$K$J$j$^$7$?!#(B

** $(B%5%^%j$G0lDj0J>e?<$$%9%l%C%I$OJ,3d$5$l$^$9!#(B
   $(BJQ?t(B wl-summary-max-thread-depth $(B$G8B3&$rJQ99$G$-$^$9!#(B

** Emacs multi-tty support $(B$KBP1~$7$^$7$?!#(B
   (http://lorentey.hu/project/emacs.html)

** $(B%5%^%j$NJB$YBX$(>r7o$K(B 'size' $(B$,DI2C$5$l$^$7$?!#(B
   $(B%a%C%;!<%8%5%$%:$K$h$k%5%^%j$NJB$YBX$($,2DG=$K$J$j$^$7$?!#(B

** $(BJQ?t(B wl-refile-policy-alist $(B$OGQ;_$5$l$^$7$?!#(B

** $(B%P%C%A=hM}MQ$N%b%8%e!<%k$,?7$?$KDI2C$5$l$^$7$?!#(B

** $(B%^%k%A%U%)%k%@$H%*%j%8%J%k%U%)%k%@$N>uBV$,F14|$5$l$k$h$&$K$J$j$^$7$?!#(B
   $(BNc$($P!"(B+inbox $(B$NL$FI>pJs$,!"(B*+inbox,+outbox $(B$K$bH?1G$5$l$^$9!#(B

** $(B4X?t(B wl-summary-resend-message $(B$OGQ;_$5$l$^$7$?!#(B
   $(B$=$NBe$o$j$K(B wl-summary-resend $(B$r;H$($P:FAw%^!<%/$rIU$1$k$3$H$,$G$-$^$9!#(B

** $(BJQ?t(B wl-delete-folder-alist $(B$O(B wl-dispose-folder-alist $(B$K(B
   $(BL>A0$,JQ99$5$l$^$7$?!#(B

** POP3 $(B%U%)%k%@$NB8:_%A%'%C%/$N%G%U%)%k%H5sF0$r4JN,2=(B
   elmo-pop3-exists-exactly $(B$N%G%U%)%k%HCM$r(B nil $(B$K$7$^$7$?!#(B

** RFC2449 $(B$N(B POP3 $(B3HD%$N%l%9%]%s%9%3!<%I$KBP1~$7$^$7$?!#(B
   $(BB>$N%;%C%7%g%s$,;HMQCf$G$"$k$?$a$KG'>Z$K<:GT$7$?$H$-$K$O!"%Q%9%o!<%I(B
   $(B$,%/%j%"$5$l$J$/$J$j$^$7$?!#(B

** IMAP4 $(B$K$*$$$F!"(BEXPUNGE, CHECK $(B%3%^%s%I$rHsF14|$GAw?.$9$k$h$&$K$7$^$7$?!#(B

** wl-folder-hierarchy-access-folders $(B$N=i4|CM$,JQ99$5$l$^$7$?!#(B

** $(B?7J9%U%)%k%@$N%"%/%;%9%0%k!<%W(B \"@/\" $(B$,;H$($k$h$&$K$J$j$^$7$?!#(B

** $(BA0$K;H$C$F$$$?$b$N$h$j?7$7$$(B Wanderlust $(B$r5/F0$9$k$H(B NEWS(.ja) $(B$NFbMF$r(B
   $(BI=<($7$^$9!#(B

** wl-draft-reply-*-list $(B$N=i4|CM$,JQ99$5$l$^$7$?!#(B
   $(B0JA0$N@_Dj$O!"(Bsamples/ja/dot.wl $(B$r;2>H$7$F2<$5$$!#(B

** wl-draft-reply-myself-*-list $(B$OGQ;_$5$l!"(Bwl-draft-reply-*-list $(B$KE}9g(B
   $(B$5$l$^$7$?!#(B

** $(BJV?.MQ%I%i%U%H$N%+!<%=%k$N=i4|0LCV$r;XDj$G$-$^$9!#(B
   $(BJQ?t(B wl-draft-reply-default-position $(B$r@_Dj$7$F2<$5$$!#(B

** $(B%I%i%U%H%P%C%U%!%&%#%s%I%&$NG[CV$N;XDj$N;EJ}$,JQ99$5$l$^$7$?!#(B
   wl-draft-buffer-style $(B$H(B wl-draft-reply-buffer-style $(B$K(B keep,full,split
   $(B$N$$$:$l$+$r;XDj$7$^$9!#(B

** $(B?75,(B hook
   wl-draft-reply-hook
   wl-summary-reply-hook
   wl-draft-forward-hook
   wl-summary-forward-hook
   wl-draft-kill-pre-hook
   wl-summary-resend-hook

** $(BGQ;_$5$l$?(B hook
   wl-reply-hook

** $(B?75,(B face

   wl-highlight-summary-disposed-face
   wl-highlight-summary-prefetch-face
   wl-highlight-summary-resend-face
   wl-highlight-summary-answered-face
   wl-highlight-action-argument-face

** $(BGQ;_$5$l$?(B face

   wl-highlight-refile-destination-face
   (wl-highlight-action-argument-face $(B$KJQL>(B)
") ((2 10 1) . "* 2.10.0 $(B$+$i(B 2.10.1 $(B$X$NJQ99E@(B
  2.10.1 $(B$O(B 2.10.0 $(B$N%P%0=$@5HG$G$9!#(B

** print-length $(B$d(B print-level $(B$,(B Non-nil $(B$N$H$-$K(B msgdb $(B$,2u$l$kLdBj$,(B
   $(B=$@5$5$l$^$7$?!#(B

** $(B%Q%$%W%U%)%k%@$K$*$$$F(B wl-summary-pack-number $(B$,$&$^$/F0$+$J$+$C$?(B
   $(B$?$a8z$+$J$/$7$F$"$j$^$9!#I,MW$G$"$l$P<h$j9~$_@h%U%)%k%@$NJ}$G<B9T(B
   $(B$7$F2<$5$$!#(B

** wl-folder-move-cur-folder $(B$,8z$$$F$$$J$+$C$?LdBj$,=$@5$5$l$^$7$?!#(B

** Meadow $(B>e$G(B wl-draft-reedit $(B$,$&$^$/F0$+$J$$LdBj$,=$@5$5$l$^$7$?!#(B

** wl-summary-pack-number $(B$,(B Maildir $(B$d(B shimbun $(B%U%)%k%@$GF0$+$J$$(B
   $(BLdBj$,=$@5$5$l$^$7$?!#(B

** $(B=EMW%^!<%/$D$-%a%C%;!<%8$N%-%c%C%7%e$KBP$9$kJ]8n5!G=$,F/$+$J$$(B
   $(BIT6q9g$,=$@5$5$l$^$7$?!#(B

** wl-summary-line-format $(B$N(B %# $(B$GBg$-$J?tCM$r@5$7$/07$($J$$IT6q9g(B
   $(B$,=$@5$5$l$^$7$?!#(B

** SMTP AUTH $(B$GG'>Z0J30$N%(%i!<$G$b%Q%9%o!<%I>C5n$5$l$kIT6q9g$,=$(B
   $(B@5$5$l$^$7$?!#(B

** wl-message-buffer-prefetch-folder-type-list,
   wl-message-buffer-prefetch-idle-time,
   wl-message-buffer-prefetch-depth $(B$N%G%U%)%k%HCM$,JQ99$5$l$^$7$?!#(B

** XEmacs without mule $(B$G%3%s%Q%$%k$G$-$J$$LdBj$,=$@5$5$l$^$7$?!#(B
") ((2 10 0) . "* 2.8.1 $(B$+$i(B 2.10.0 $(B$X$NJQ99E@(B

** $(B%5%^%j9T$NI=<(7A<0$rJQ99$G$-$k$h$&$K$J$j$^$7$?!#(B
   wl-summary-line-format $(B$G=q<0$r@_Dj$G$-$^$9!#%U%)%k%@Kh$K=q<0$rJQ$($?$$(B
   $(B>l9g$O(B wl-folder-summary-line-format-alist $(B$rMQ$$$F2<$5$$!#(B

** $(B%I%i%U%H%U%)%k%@$X$NJ]B87A<0$,JQ99$5$l$^$7$?!#(Bwl-draft-save $(B$N:]$K$O(B
   $(B%(%s%3!<%I$7$FJ]B8$5$l$^$9!#(B

** elmo-split $(B$,?7@_$5$l$^$7$?!#M?$($?%k!<%k$K1h$C$F(B procmail $(BIw$K%a%C%;(B
   $(B!<%8$r?6$jJ,$1$k$3$H$,$G$-$^$9!#(B

** $(B%P%C%U%!%W%j%U%'%C%A$,<BAu$5$l$^$7$?!#(Bwl-message-buffer-prefetch-depth
   $(B$N?t$@$1!"%a%C%;!<%8$r%P%C%U%!$K@hFI$_$7$^$9!#(B

** elmo-dop-queue-flush $(B$O7R$,$C$F$$$k%]!<%H$K4X$9$k%-%e!<$r(B flush $(B$7$^$9!#(B

** $(B?7$7$$%U%l!<%`$r3+$$$F(B Wanderlust $(B$r5/F0$G$-$k$h$&$K$J$j$^$7$?!#(B
   (autoload 'wl-other-frame \"wl\" \"Wanderlust on new frame.\" t)
   $(B$N$h$&$K@_Dj$7$F2<$5$$!#(B

** $(B%U%)%k%@%b!<%I$+$i!"M?$($i$l$?>r7o$rK~$?$9%a%C%;!<%8$+$i$J$k2>A[%U%)%k%@(B
   $(B$X0\F0$G$-$^$9(B (wl-folder-virtual)$(B!#(B\"V\" $(B$K%P%$%s%I$5$l$F$$$^$9!#(B

** $(B%U%)%k%@%b!<%I$G!"M?$($i$l$?>r7o$rK~$?$9%a%C%;!<%8$r4^$`%U%)%k%@$rC5$;$k(B
   $(B$h$&$K$J$j$^$7$?(B (wl-folder-pick)$(B!#(B\"?\" $(B$K%P%$%s%I$5$l$F$$$^$9!#(B 

** $(B%"%/%;%9%0%k!<%W%U%)%k%@$N2~L>$,=PMh$k$h$&$K$J$j$^$7$?!#(B

** $(B?75,%5%^%j$KBP$9$k%9%l%C%II=<($N(B ON/OFF $(B$r;XDj$G$-$k$h$&$K$J$j$^$7$?!#(B
   wl-summary-default-view, wl-summary-default-view-alist $(B$r@_Dj$7$F2<$5$$!#(B

** $(B%9%F%#%C%-!<%5%^%j$r(B q $(B$d(B g $(B$GH4$1$k:]$K!"0l;~E*%^!<%/$rJ];}$9$k$h$&$K(B
   $(B$J$j$^$7$?!#(B

** $(B%9%F%#%C%-!<%5%^%j$K4X$9$k%-!<%P%$%s%I$,JQ99$K$J$j$^$7$?!#(B
   $(B%5%^%j$G(B C-u g $(B$9$k$H(B C-u q $(B$HF1MM$K%5%^%j$rGK4~$7$^$9!#%5%^%j$d%U%)%k%@(B
   $(B%b!<%I$+$i(B G $(B$G%5%^%j$K0\F0$9$k$H!"?75,%5%^%j$,%9%F%#%C%-!<$K$J$j$^$9!#(B

** C-cC-n $(B$d(B C-cC-p $(B$G%5%^%j%P%C%U%!4V$r=d2s$G$-$^$9!#(B

** $(B%j%9%H(B wl-folder-hierarchy-access-folders $(B$N3FMWAG$O!"%"%/%;%9%0%k!<%W$K(B
   $(B$D$$$F$N@55,I=8=$K$J$j$^$7$?(B($(B$3$l$^$G$O@53N$J%0%k!<%WL>$G$7$?(B)$(B!#(B

** $(B%I%i%U%H$N%X%C%@ItJ,$G(B C-a $(B$9$k$H!"9TF,$b$7$/$O%X%C%@$N@hF,$K%+!<%=%k$,(B
   $(B0\F0$7$^$9!#(B

** $(B%+%W%;%k2=(B Blind Carbon Copy $(B$rAw$l$k$h$&$K$J$j$^$7$?!#(B
   $(B%G%U%)%k%H$N%U%#!<%k%IL>$O(B \"Ecc:\" $(B$G$9!#(B

** $(B%I%i%U%H$N(B C-c C-y $(B$G%j!<%8%g%s$r0zMQ$G$-$k$h$&$K$J$j$^$7$?!#(B
   transient-mark-mode (Emacs) $(B$b$7$/$O(B zmacs-regions (XEmacs)
   $(B$,(B Non-nil $(B$G!"%j!<%8%g%s$,M-8z$N$H$-$K:nMQ$7$^$9!#(B

** $(B%^%k%A%Q!<%H$N%a%C%;!<%8$+$i%Q!<%H$r:o=|$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(B%a%C%;!<%8%P%C%U%!$G(B \"D\" $(B$K%P%$%s%I$5$l$F$$$^$9!#(B

** $(B%K%e!<%95-;v$rEj9F$9$k%5!<%P$r4JC1$K@_Dj$G$-$k$h$&$K$J$j$^$7$?!#(B
   Info $(B$NNc$K=,$C$F(B wl-nntp-posting-config-alist $(B$r@_Dj$7$F2<$5$$!#(B

** $(BJQ?t(B wl-draft-reply-with-argument-list $(BEy$G!"4X?t$NJV$jCM$+$i08@h$r(B
   $(B7h$a$i$l$k$h$&$K$J$j$^$7$?!#(B

** $(B4X?t(B wl-draft $(B$N%$%s%?!<%U%'!<%9$,JQ99$5$l$^$7$?!#(B
   $(B:G=i$KMQ0U$9$k%X%C%@$r4XO"%j%9%H$N7A$GEO$9$h$&$K$J$j$^$7$?!#(B

** wl-generate-mailer-string-function $(B$N;HMQK!$,JQ99$K$J$j$^$7$?!#(B
   User-Agent $(B%U%#!<%k%I$KF~$kJ8;zNs$rJV$94X?t$r;XDj$7$F2<$5$$!#(B

** Reference Card (doc/wl-refcard-ja.tex) $(B$K<g$J%-!<A`:n$rNs5s$7$^$7$?!#(B

** $(B$=$NB>$$$/$D$+$N=$@5!#(B
") ((2 8 0) . "* 2.6.1 $(B$+$i(B 2.8.0 $(B$X$NJQ99E@(B

** Nemacs, Mule 2.3 based on Emacs 19.28 $(B$O%5%]!<%H$5$l$J$/$J$j$^$7$?!#(B

** FLIM 1.14.2 $(B0JA0$N(B FLIM $(B$G$O$&$^$/F0$+$J$$>l9g$,$"$j$^$9!#(B
   FLIM 1.14.3 $(B0J9_$*$h$SBP1~$7$?(B SEMI $(B$r%$%s%9%H!<%k$7$F$/$@$5$$!#(B

** make check $(B$G4JC1$J4D6-%F%9%H$,$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(BL>A0JQ99Ey$G;H$o$l$J$/$J$C$?JQ?t$r(B .wl $(BEy$G@_Dj$7$F$$$k$H!"7Y9p$,I=<($5$l(B
   $(B$^$9!#%a%C%;!<%8$r;29M$K$7$F!"@_Dj$rJQ99$7$F$/$@$5$$!#(B
   $(B$b$72?$i$+$NM}M3$G7Y9p$NI=<($rM^@)$7$?$$>l9g$K$O!"JQ?t(B
   elmo-obsolete-variable-show-warnings $(B$r(B nil $(B$K$7$F$/$@$5$$!#(B

** $(B?75,FbIt%U%)%k%@(B 'sendlog $(B$,DI2C$5$l$^$7$?!#(B

** $(B?75,%U%)%k%@(B shimbun $(B%U%)%k%@$,DI2C$5$l$^$7$?!#(B

   $(B=q<0(B: '@' '$(B2>A[%5!<%PL>(B' '.' '$(B%0%k!<%WL>(B'

** $(B?75,%U%)%k%@(B namazu $(B%U%)%k%@$,DI2C$5$l$^$7$?!#(B

   $(B=q<0(B:  '[' 'namazu $(B8!:w<0(B' ']' [ 'namazu index $(B$N%Q%9!J@dBP%Q%9!K(B' ]

** $(B%Q%$%W%U%)%k%@$G%5!<%P$K%a%C%;!<%8$r;D$9$3$H$,$G$-$k$h$&$K$J$j$^$7$?(B
   $(B<!$N%"%/%;%9;~$K$O!"?7$7$$%a%C%;!<%8$N$_%3%T!<$7$^$9!#(B

   $(B=q<0(B:  '|' '$(B<h$j9~$_85(B' '|:' '$(B<h$j9~$_@h(B'

** $(B%"%I%l%9%^%M!<%8%c$,?7@_$5$l$^$7$?(B(C-c C-a $(B$G5/F0(B)$(B!#(B
   $(B%"%I%l%9D"$NJT=8$r$7$?$j!"$=$3$+$i%I%i%U%H$K08@h$rF~NO$9$k$3$H$,$G$-$^$9!#(B

** ACAP (RFC2244) $(B$KBP1~$7$^$7$?(B($(B<B83E*(B)$(B!#(B

** IMAP4 $(B$N%a%C%;!<%8$r%Q!<%HKh$K%-%c%C%7%e$H$7$FJ]B8$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(B5pBg$J%Q!<%H$r%9%-%C%W$7$?>l9g$G$b!"%9%-%C%W$7$?%Q!<%H0J30$O(B
   $(B%*%U%i%$%s>uBV$GFI$_JV$9$3$H$,$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(B%a%C%;!<%8$N%W%j%U%'%C%A$G%a%C%;!<%8%S%e!<$^$G:n@.$9$k$h$&$K$J$j$^$7$?!#(B
   $(B%W%j%U%'%C%A$5$l$?%a%C%;!<%8$NI=<($,9bB.2=$5$l$^$7$?!#(B

** $(B%a%C%;!<%8%P%C%U%!!"%I%i%U%H%P%C%U%!$GD9$$9T$N@^$jJV$7$r@_Dj$G$-$k$h$&$K(B
   $(B$J$j$^$7$?!#JQ?t(B wl-message-truncate-lines, wl-draft-truncate-lines $(B$,(B
   non-nil $(B$J$i!"$=$l$>$l$K$D$$$FD9$$9T$r(B window $(BI}$G@Z$j=L$a$^$9!#(B

** $(B%*!<%W%K%s%0%G%b$K;H$o$l$k%S%C%H%^%C%W2hA|$O(B wl-demo.elc $(B$+$i<h$j=|$+$l!"(B
   wl-icon-directory $(B$+$iFI$_9~$`$h$&$K$J$j$^$7$?!#(B
   $(B%/%j%9%^%9$N;~4|$K$OFCJL$J2hA|$,I=<($5$l$^$9(B :)

** elmo $(B%b%8%e!<%k$,A4BNE*$K=q$-D>$5$l$^$7$?!#(B

** elmo $(B$N%P%C%/%(%s%I$K0MB8$7$?JQ?t$O(B \"elmo-$(B%P%C%/%(%s%IL>(B-*\" 
   $(B$H$$$&L>A0$KJQ99$5$l$^$7$?!#(B
   $(BNc$($P!"(B elmo-default-imap4-server $(B$,(B elmo-imap4-default-server $(B$K(B
   $(BJQ99$5$l$F$$$^$9!#(B

** xxx-func $(B$H$$$&L>A0$NJQ?t$O(B xxx-function $(B$H$$$&L>A0$KJQ99(B $(B$5$l$^$7$?!#(B

** X-Face utility 1.3.6.12 $(B0JA0$O%5%]!<%H$5$l$J$/$J$j$^$7$?!#(B
   $(BI,MW$J$i(B X-Face utility 1.3.6.13 $(B0J9_$r%$%s%9%H!<%k$7$F$/$@$5$$!#(B

** plugged $(B%b!<%I$G!"(Bstream-type $(B$,0c$&$b$N$OJL%(%s%H%j$H$7$F07$o$l$k$h$&$K(B
   $(B$J$j$^$7$?!#(B

** $(B%"!<%+%$%V(B, $(B%^%k%A%U%)%k%@MQ$N(B msgdb path $(B$,JQ99$5$l$^$7$?!#(B
   $(B$=$N$^$^$G$bLdBj$"$j$^$;$s$,!"%G%#%9%/$KL5BL$J%G!<%?$r;D$7$?$/$J$$J}$O(B
   .elmo/multi, .elmo/archive $(B0J2<$r$"$i$+$8$a:o=|$7$F$*$$$F$/$@$5$$!#(B

** xxx-dir $(B$H$$$&L>A0$NJQ?t$O(B xxx-directory $(B$H$$$&L>A0$KJQ99$5$l$^$7$?!#(B
   $(BNc$($P!"(Bwl-icon-dir $(B$O(B wl-icon-directory $(B$KJQ99$5$l$F$$$^$9!#(B
   Emacs21 $(B$G(B logo $(BI=<($J$I$N@_Dj$r$7$F$$$kJ}$OFC$KCm0U$7$F$/$@$5$$!#(B

** elmo-cache-dirname $(B$rGQ;_$7$F(B elmo-cache-directory $(B$r?7@_$7$^$7$?!#(B
   elmo-cache-directory $(B$r@_Dj$9$k$3$H$K$h$C$F%-%c%C%7%e$@$1$rA4$/JL$N(B
   $(B%G%#%l%/%H%j$KCV$/$3$H$,$G$-$^$9!#(B

** elmo-enable-disconnected-operation $(B$N%G%U%)%k%HCM$,(B t $(B$K$J$j$^$7$?!#(B
   $(B%*%U%i%$%s>uBV$G$b%a%C%;!<%8$,%-%c%C%7%e$5$l$F$$$l$P!"$"$kDxEY$N(B
   $(B%a%C%;!<%8A`:n$,2DG=$G$9!#(B

** \"$\" $(B%^!<%/$NIU$$$?%a%C%;!<%8$O!"%a%C%;!<%8$N<BBN$,>C$($?>l9g$K$O%5%^%j$+$i(B
   $(B$b>C$($k$h$&$K$J$j$^$7$?!#(B
   \"$\" $(B%^!<%/$NIU$$$?%a%C%;!<%8$r8+D>$7$?$$>l9g$O(B 'mark $(B%U%)%k%@$r;2>H$7$F(B
   $(B$/$@$5$$!#(B
") ((2 6 1) . "* 2.6.0 $(B$+$i(B 2.6.1 $(B$X$NJQ99E@(B
  2.6.1 $(B$O(B 2.6.0 $(B$N=$@5HG$G$9!#(B

** Emacs 21 $(B$G(B Recursive load... $(B$H=P$kIT6q9g$,=$@5$5$l$^$7$?!#(B

** XEmacs 21.1 $(B$GJ8;z2=$1$9$kLdBj$,=$@5$5$l$^$7$?!#(B

** XEmacs $(B$G(B IMAP4 $(B$rMQ$$$k$H%W%m%0%l%9%P!<$,=P$C$Q$J$7$K$J$kLdBj$,(B
   $(B=$@5$5$l$^$7$?!#(B

** X- $(B$G;O$^$k%U%#!<%k%I$N8!:w$,$G$-$J$$LdBj$,=$@5$5$l$^$7$?!#(B

** $(B$=$NB>$$$/$D$+$N=$@5!#(B
") ((2 6 0) . "* 2.4.1 $(B$+$i(B 2.6.0 $(B$X$NJQ99E@(B

** FLIM 1.13.x $(B$O%5%]!<%H$5$l$J$/$J$j$^$7$?!#(B
   FLIM 1.14.1 $(B0J9_$r%$%s%9%H!<%k$7$F$/$@$5$$!#(B

** $(B%U%)%k%@!"%5%^%j$rJL%U%l!<%`$G5/F0$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(B?75,JQ?t(B wl-folder-use-frame$(B!"(Bwl-summary-use-frame $(B$,(B non-nil $(B$J$i(B
   $(B$=$l$>$l%U%)%k%@!"%5%^%j$rJL%U%l!<%`$K3+$-$^$9(B($(B%G%U%)%k%H$O(B nil)$(B!#(B

** $(B%5%^%j$G$N(B 'N' $(B$d(B 'P' $(B$K$h$k%+!<%=%k0\F0$,9bB.2=$5$l$^$7$?!#(B

** $(B%m!<%+%k%U%)%k%@$rBP>]$H$7$?!"(Blast $(B$*$h$S(B first $(B>r7o$N%U%#%k%?%U%)%k%@(B
   ($(BNc$($P!"(B/last:100/+inbox $(B$N$h$&$J%U%)%k%@(B) $(B$N%A%'%C%/$,9bB.2=$5$l$^$7$?!#(B

** POP, IMAP $(B$GBg$-$J%a%C%;!<%8$r<h$j4s$;$k$H$-$K!"(B
   $(B?JD=$,I=<($5$l$k$h$&$K$J$j$^$7$?!#(B

** $(B%5%^%j$N?'$E$1$,%*%s%G%^%s%I$K$J$j$^$7$?(B(Emacs $(B$N$_(B)$(B!#(B
   $(B?75,JQ?t(B wl-summary-lazy-highlight $(B$,(B non-nil $(B$J$i%5%^%j$NI=<(ItJ,$N$_$r(B
   $(B<+F0E*$K?'IU$1$7$^$9(B (Emacs $(B$N$_(B)$(B!#(B

** biff $(B$NDLCNJ}K!$,%+%9%?%^%$%:2DG=$K$J$j$^$7$?!#(B
   $(B?75,%U%C%/(B wl-biff-notify-hook, wl-biff-unnotify-hook $(B$G@_Dj$G$-$^$9!#(B
   $(BNc(B: (add-hook wl-biff-notify-hook 'ding)

** $(BB?$/$N%P%0%U%#%C%/%9(B
") ((2 4 1) . "* 2.4.0 $(B$+$i(B 2.4.1 $(B$X$NJQ99E@(B
  2.4.1 $(B$O(B 2.4.0 $(B$N=$@5HG$G$9!#(B

** FLIM 1.14.x $(B>e$G$bF0:n$9$k$h$&$K$J$j$^$7$?!#(B

** POP before SMTP $(B$G(B POP $(B%3%M%/%7%g%s$,@Z$l$J$$IT6q9g$,=$@5$5$l$^$7$?!#(B

** IMAP4 $(B$G$N@8%Q%9%o!<%I$K$h$kG'>Z$N;XDjJ}K!$,JQ99$K$J$j$^$7$?!#(B

$(B$3$l$^$G!"(BIMAP4 $(B$G@8%Q%9%o!<%I$NG'>Z(B(LOGIN $(B%3%^%s%I$K$h$k%m%0%$%s(B)$(B$r$9$k$K$O!"(B
$(BJQ?t(B elmo-default-imap4-authenticate-type $(B$K(B 'plain ($(B$^$?$O(B nil) 
$(B$r@_Dj$9$k$3$H$K$J$C$F$$$^$7$?$,!"(B'clear ($(B$^$?$O(B nil)$(B$KJQ99$5$l$^$7$?!#(B
$(BNc$($P!"(B
\(setq elmo-default-imap4-authenticate-type 'plain)
$(B$H$$$&@_Dj$O(B
\(setq elmo-default-imap4-authenticate-type 'clear)
$(B$KJQ99$9$kI,MW$,$"$j$^$9!#(B
") ((2 4 0) . "* 1.1.1 $(B$+$i(B 2.4.0 $(B$X$NJQ99E@(B

** $(B%P!<%8%g%sHV9f(B
$(B%P!<%8%g%sHV9f$NIU$1J}$,JQ$o$j$^$7$?!#(B
$(B$3$l$^$G!"(B1.x $(B$,0BDjHG!"(B2.0.x$(B!A(B2.2.x $(B$,3+H/HG$H$J$C$F$$$^$7$?$,!"(B2.3.0 
$(B0J9_$O!"BhFsHV9f$,6v?t$J$i0BDjHG!"4q?t$J$i3+H/HG$H$J$j$^$7$?!#$3$N%P!<(B
$(B%8%g%sHV9f$NIU$1J}$O0lHLE*$J%*!<%W%s%=!<%93+H/$N47=,$K4p$E$/$b$N$G$9!#(B

$(B8x3+(B CVS $(B%5!<%P(B cvs.m17n.org $(B>e(B $(B$G$O!"44$,(B beta ($(B3+H/(B)$(BHG!"(B
$(B;^$,(B stable ($(B0BDj(B)$(BHG(B ($(B;^L>$O!"(B2.4.x $(B$J$i(B wl-2_4) $(B$H$J$j$^$9!#(B

** $(B%$%s%9%H!<%k(B

*** FLIM 1.12 $(B$O%5%]!<%H$5$l$J$/$J$j$^$7$?!#(B
$(B$/$o$7$/$O(B INSTALL.ja $(B$r8fMw2<$5$$!#(B

*** APEL 10.2 $(B0J9_$,I,MW$K$J$j$^$7$?!#(B
tm-8 $(B$r;HMQ$9$kJ}$OFC$K$4Cm0U$/$@$5$$!#(B

** $(B?75!G=(B

*** LDAP $(B%5%]!<%H(B
LDAP $(B%5!<%P!<$H@\B3$7!"%"%I%l%9$NJd40$r9T$($^$9!#(B
$(BJQ?t(B wl-use-ldap $(B$,(B non-nil $(B$K@_Dj$5$l$F$$$k$H(B LDAP $(B$rMxMQ$7$^$9(B
\($(B=i4|@_Dj$O(B nil)$(B!#(B

*** POP3 $(B%U%)%k%@$G(B UIDL $(B%5%]!<%H(B
POP3 $(B%U%)%k%@$G%5%^%j$N>uBV$rJ]B8$G$-$k$h$&$K$J$j!"%"%/%;%9$,9bB.2=$5$l$^$7$?!#(B
$(BJQ?t(B elmo-pop3-use-uidl $(B$,(B non-nil $(B$K@_Dj$5$l$F$$$k$H(B UIDL $(B$r;HMQ$7$^$9(B
\($(B=i4|@_Dj$O(B t)$(B!#(B

*** Emacs 21 $(B%5%]!<%H(B
Standard Emacs 21 $(B$N%5%]!<%H$r3+;O$7$^$7$?!#(BWanderlust $(B$N$[$H$s(B
$(B$I$N%U%l!<%`$K!"(BXEmacs $(B$HF1$8$h$&$K%D!<%k%P!<$d%"%$%3%s2hA|$rI=(B
$(B<($7$^$9!#(B

*** biff $(B5!G=(B
$(B0lDj;~4V$*$-$K%5!<%P$K%a!<%k$,FO$$$F$$$k$+3NG'$7$^$9!#(B
$(BFO$$$F$$$l$P%b!<%I%i%$%s$KI=<($7!"%U%)%k%@0lMw%b!<%I$r99?7$7$^$9!#(B

*** expire-hide
$(B5-;v<+BN$O>C$9$3$H$J$/!"%5%^%j$K8+$($k5-;v?t$r0lDj$KJ]$D$3$H$,$G(B
$(B$-$k$h$&$K$J$j$^$7$?!#$R$H$D$N%U%)%k%@$KBgNL$K5-;v$rN/$a$F$$$k>l(B
$(B9g$G$b!"B.EYDc2<$rM^$($k$3$H$G$-$^$9!#(B

*** $(B%9%l%C%I=$I|5!G=(B
$(B%5%V%8%'%/%H$+$i?dB,$7$?%9%l%C%I$N<+F0$N$D$J$.D>$7!"5Z$S(B
$(B<jF0$G$N$D$J$.D>$7(B($(B%5%^%j$G(B M-w ($(B%3%T!<(B)$(B$H(B C-y ($(B%Z!<%9%H(B)) $(B$,2DG=$K$J$j$^$7$?!#(B

*** $(B%Q%9%o!<%I$N%?%$%^@_Dj(B
$(BJQ?t(B elmo-passwd-life-time $(B$G@_Dj$G$-$^$9!#(B
\(nil $(B$J$i%?%$%^$J$7!#=i4|@_Dj$O(B nil)$(B!#(B

*** killed-list
NNTP $(B%U%)%k%@$G:o=|$7$?%a%C%;!<%8$O(B killed-list $(B$KJ]B8$7$^$9!#(B
killed-list $(B$K$"$k%a%C%;!<%8$O%5!<%P>e$K$bB8:_$7$J$$$+$N$h$&$K07(B
$(B$$$^$9!#JQ?t(B elmo-use-killed-list $(B$,(B non-nil $(B$J$i(B killed-list $(B$r(B
$(B;HMQ$7$^$9(B($(B%G%U%)%k%H$O(B t)$(B!#(B
$(B$3$l$K$h$C$F(B NNTP $(B$rMxMQ$7$?%Q%$%W%U%)%k%@$b<B8=$G$-$k$h$&$K$J$j$^$7$?!#(B

*** Maildir $(B$G(B pack ($(BHV9f5M$a(B) $(B$,$G$-$k$h$&$K$J$j$^$7$?!#(B
Maildir $(B$N%5%^%j$G(B M-x wl-summary-pack-number $(B$r<B9T$9$k$H%a%C%;!<%8HV9f$r(B
1 $(B$+$i=g$K?6$jD>$7$^$9!#(B

** $(B8!:w(B

*** $(B%U%#%k%?%U%)%k%@$KJ#;($J>r7o;XDj$r;XDj$G$-$k$h$&$K$J$j$^$7$?!#(B
AND $(B>r7o!"(BOR $(B>r7o!"H]Dj>r7o!"$*$h$S$=$l$i$NAH9g$;$r;XDj$G$-$^$9!#(B
$(B$3$l$K$H$b$J$$!">r7o;XDjItJ,$N%7%s%?%C%/%9$,JQ99$5$l$^$7$?!#(B
$(B$/$o$7$/$O(B Info $(B$r8fMw2<$5$$!#(B

$(BCm0U!'(B1.1.1 $(B$+$i0\9T$5$l$kJ}$X(B
$(B>e5-JQ99$KH<$$!"%U%#%k%?%U%)%k%@$N(B msgdb $(B$NCV$->l=j$,JQ$o$j$^$7$?!#(B
$(B$3$N$?$a!"=>Mh$N(B msgdb $(B$OITMW$H$J$j$^$9!#$=$N$^$^$G$bLdBj$"$j$^$;$s$,!"(B
$(B%G%#%9%/$KL5BL$J%G!<%?$r;D$7$?$/$J$$J}$O(B .elmo/filter/ $(B0J2<$r(B
$(B$"$i$+$8$a:o=|$7$F$*$$$F$/$@$5$$!#(B

*** NNTP $(B$G$N8!:w5!G=$,6/2=$5$l$^$7$?!#(B
NNTP $(B$KBP$9$k%U%#%k%?%U%)%k%@$r:n$l$k$h$&$K$J$j$^$7$?!#(B
\(NNTP $(B%5!<%P$,(B XHDR $(B%3%^%s%I$KBP1~$7$F$$$k>l9g$N$_(B)

*** $(B%5%^%j$G$N(B Pick$(B!"(BVirtual $(B$GJ#9g>r7o$rF~NO$G$-$k$h$&$K$J$j$^$7$?!#(B
AND $(B>r7o$d(B OR $(B>r7o$bF~NO$G$-$^$9!#(B
$(BF~NOJ}K!$O!"%U%#!<%k%IL>$N$+$o$j$K(B 'AND' $(B$d(B 'OR' $(B$rF~NO$9$k$@$1$G$9!#(B

** $(B@\B3!&G'>Z(B

*** elmo-default-*-authenticate-type $(B$O%7%s%\%k$G@_Dj$9$k$h$&$K$J$j$^$7$?!#(B
$(BNc$($P!"(B
\(setq elmo-default-imap4-authenticate-type \"cram-md5\")
$(B$H$$$&@_Dj$O!"(B
\(setq elmo-default-imap4-authenticate-type 'cram-md5)
$(B$KJQ99$9$kI,MW$,$"$j$^$9!#(B

*** stream-type $(B$NDj5AJ}K!$rJQ99$7$^$7$?!#(B
$(BJQ?t(B elmo-network-{imap4-,pop3-,nntp-,}stream-type-alist $(B$G@_Dj2DG=$G$9!#(B
SSL $(B4XO"$N$$$/$D$+$NJQ?t$,GQ;_$5$l$^$7$?(B($(B2~L>(B)$(B!#(B 
$(B$^$?!"?7$?$K(B \"!socks\" $(B$G=*$o$k%M%C%H%o!<%/7O%U%)%k%@(B(IMAP4, NNTP, POP3)$(B$O(B
SOCKS $(B7PM3$G%"%/%;%9$5$l$k$h$&$K$J$j$^$7$?!#(B

** $(B%I%i%U%H(B

*** group-list $(B$KBP1~$7$^$7$?!#(B
$(B08@h$K(B Group: foo@gohome.org, bar@gohome.org; $(B$N$h$&$K=q$1$k$h$&$K(B
$(B$J$j$^$7$?!#JQ?t(B wl-draft-remove-group-list-contents $(B$,(B t $(B$J$i(B
group-list $(B$NFbMF$r:o=|$7$FAw?.$7$^$9!#(B

*** $(B%I%i%U%H$N%W%l%S%e!<$G<u<h?M$N%"%I%l%9$,%_%K%P%C%U%!$KI=<($5$l$^$9!#(B
group-list $(B$K$bBP1~$7$F$$$^$9!#(B

*** $(B=i4|@_Dj$G(B Reply-To: $(B$r9MN8$9$k$h$&$K$J$j$^$7$?!#(B
wl-draft-reply-without-argument-list $(B$N=i4|@_Dj$G!"(BReply-To: 
$(B%U%#!<%k%I$O(B To: $(B$XA^F~$9$k@_Dj$K$J$j$^$7$?!#(B

*** $(B<+J,$N%a!<%k$X$NJV?.%k!<%k(B
$(BJQ?t(B wl-draft-reply-myself-with-argument-list,
wl-draft-reply-myself-without-argument-list $(B$G<+J,$,=P$7$?%a!<%k(B
$(B$X$NJV?.$9$k$H$-$N%k!<%k$,@_Dj$G$-$^$9!#(B

*** $(BJV?.%"%I%l%9$K%U%k%M!<%`(B
$(BJQ?t(B wl-draft-reply-use-address-with-full-name $(B$,(B non-nil $(B$J$iJV(B
$(B?.%"%I%l%9$K%U%k%M!<%`$,F~$j$^$9!#(B($(B%G%U%)%k%H$O(B t)$(B!#(B 

*** In-Reply-To: $(B%U%#!<%k%I$N7A<0$rJQ99$7$^$7$?!#(B
draft-ietf-drums-msg-fmt-09.txt $(B$K=>$&$h$&$K$J$j$^$7$?!#(B

** $(B$=$NB>$NJQ99E@(B

*** $(B%9%l%C%I$N9bB.2=$HB?$/$N%P%0%U%#%C%/%9!#(B

*** $(BJQ?tL>$NJQ99!#(B
wl-refile-guess-func-list => wl-refile-guess-functions
wl-summary-temp-above => wl-summary-target-above

*** wl-fcc $(B$K4X?t$r@_Dj$G$-$^$9!#(B
$(B7nKh$K%U%)%k%@$rJQ$($?$$>l9g$J$I$K;HMQ$G$-$^$9!#(B

*** elmo-search-mime-charset $(B$OGQ;_$5$l$^$7$?!#(B
charset $(B$OF~NOJ8;zNs$+$iH=Dj$5$l$^$9!#(B

*** $(BE>Aw;~$KM>7W$J%X%C%@$r:o=|$7$^$9!#(B
$(BJQ?t(B wl-ignored-forwarded-headers $(B$G!"E>Aw;~$K:o=|$9$k%X%C%@$r@_(B
$(BDj$G$-$^$9!#(B

*** wl-highlight-group-folder-by-numbers $(B$OGQ;_$5$l$^$7$?!#(B
wl-highlight-folder-by-numbers $(B$K2~L>$5$l!"CM$K1~$8$F0J2<$N0UL#$r;}$D$h$&$K(B
$(B$J$j$^$7$?!#(B
  `t'   $(B!'9TA4BN$K%a%C%;!<%8?t$K1~$8$??'$rIU$1$^$9!#(B
  `nil' $(B!'%U%)%k%@$N>uBV$K1~$8$??'$rIU$1$^$9!#(B
   $(B?t;z(B ($(BNc$($P(B `1') $(B!'%a%C%;!<%8?t$H%U%)%k%@$N>uBV$NN>J}$K1~$8$??'$rIU$1$^$9!#(B

*** $(B%a%C%;!<%8%P%C%U%!$G$N%X%C%@I=<($r@)8f$G$-$^$9!#(B
$(BJQ?t(B wl-message-ignored-field-list,
wl-message-visible-field-list $(B$G!"(BWanderlust $(B%l%Y%k$G@_Dj$,2DG=(B
$(B$K$J$j$^$7$?!#(B($(B:#$^$G$O(B SEMI $(B$G@_Dj$9$kI,MW$,$"$j$^$7$?(B)

*** DEMO $(B$NI=<(J}K!$,JQ$o$j$^$7$?!#(B
$(B%+%i!<$N%T%C%/%9%^%C%W$GMxMQ$9$k?'?t$,:o8:$5$l$^$7$?!#(B
$(B$^$?!"J8;z$N$_$7$+I=<($G$-$J$$4D6-$G$b$=$l$J$j$N%G%b$,I=<($5$l$k$h$&$K(B
$(B$J$j$^$7$?!#(B
") ((1 1 1) . "* 1.1.0 $(B$+$i(B 1.1.1 $(B$X$NJQ99E@(B
  1.1.1 $(B$O(B 1.1.0 $(B$N%P%0=$@5HG$G$9!#$$$/$D$+$N:Y$+$$=$@5$,2C$o$C$F$$$^$9!#(B

** CVS $(B%5!<%P>e$G$N3+H/$,;O$a$i$l$^$7$?!#(B

** $(B%G%#%l%/%H%j9=@.$,$+$o$j$^$7$?!#(B

*** 00README, 00README.ja $(B$O(B README, README.ja $(B$KJQ99$5$l$^$7$?!#(B

*** wl-* $(B$N%U%!%$%k$O(B 'wl' $(B%G%#%l%/%H%j$K0\F0$7$^$7$?!#(B

** wl-refile-rule-alist $(B$N5-=RJ}K!$,3HD%$5$l$^$7$?(B($(B0JA0$H8_49@-$,$"$j$^$9(B)$(B!#(B

** progress gauge $(BI=<(5!G=$rMxMQ$9$k$h$&$K$J$j$^$7$?!#(B
progress gauge $(B$NI=<(5!G=$r$b$D(B Emacs $(B$G$O!"=hM}$N?JD=$,(B progress gauge $(B$K(B
$(BI=<($5$l$k$h$&$K$J$j$^$7$?!#(B
") ((1 1 0) . "* 1.0.3 $(B$+$i(B 1.1.0 $(B$X$NJQ99E@(B 

** $(B%$%s%9%H!<%k(B

*** tm7 $(B$O%5%]!<%H$5$l$J$/$J$j$^$7$?!#(B

$(B$/$o$7$/$O(B INSTALL.ja $(B$r8fMw2<$5$$!#(B

*** WL_PREFIX $(B$H(B ELMO_PREFIX $(B$N=i4|@_Dj$,(B \"wl\" $(B$K$J$j$^$7$?!#(B
\(defvar WL_PREFIX \"wl\")
\(defvar ELMO_PREFIX \"wl\")

$(BNc$($P!"%$%s%9%H!<%k%G%#%l%/%H%j$O!"(B
  1.0.3  /usr/local/share/emacs/site-lisp/
  1.1.0  /usr/local/share/emacs/site-lisp/wl/
$(B$H$J$j$^$9!#(B

*** Makefile $(B$NJQ?t$N%G%U%)%k%HCM$,JQ$o$j$^$7$?!#(B

EMACS   = emacs
XEMACS  = xemacs
$(XEMACS) $(B$O!"(B`package' $(B$d(B `install-package' $(B$N(B target $(B$G;2>H$5$l$^$9!#(B

*** *.el $(B%U%!%$%k$b%$%s%9%H!<%k$5$l$k$h$&$K$J$j$^$7$?!#(B

*** $(B1Q8lHG%I%-%e%a%s%H(B (wl.texi) $(B$,IU$-$^$7$?!#(B

** $(B?75!G=(B

*** Modified UTF7 $(B$,%5%]!<%H$5$l$^$7$?!#(B
$(B%f%K%3!<%I$,07$($k(B Emacs $(B$G$O!"(BIMAP4 $(B$GF|K\8l%a!<%k%\%C%/%9L>$r;XDj$G$-$^$9!#(B

*** $(B%9%3%"5!G=$,IU$-$^$7$?!#(B

*** $(B%W%i%04IM}5!G=$,IU$-$^$7$?!#(B

*** IMAP4 $(B$,$h$jHFMQE*$K$J$j$^$7$?!#(B
$(BB?$/$N(B IMAP4 $(B%5!<%P$GF0$/$h$&$K$J$j$^$7$?!#(B

*** $(B$$$/$D$+$NG'>ZJ}<0$,%5%]!<%H$5$l$^$7$?!#(B
  IMAP4: CRAM-MD5, DIGEST-MD5, STARTTLS
  POP3:  CRAM-MD5, DIGEST-MD5, SCRAM-MD5, STARTTLS
  NNTP:  STARTTLS
  SMTP:  STARTTLS

*** $(B?7$7$$%U%)%k%@7?$,2C$o$j$^$7$?!#(B
  |      $(B%Q%$%W%U%)%k%@(B     $(B%a%C%;!<%8$r<h$j9~$`%U%)%k%@$G$9!#(B
  .      Maildir $(B%U%)%k%@(B   Maildir $(B$,$R$H$D$N%U%)%k%@7?$K$J$j$^$7$?!#(B
  'cache $(B%-%c%C%7%e%U%)%k%@(B $(BFbIt%-%c%C%7%e$r%U%)%k%@$H$7$F1\Mw$G$-$^$9!#(B

*** $(B%a%C%;!<%8%P%C%U%!$N%W%j%U%'%C%A5!G=$,IU$-$^$7$?!#(B
$(BFI$s$G$$$k4V$K<!$N%a%C%;!<%8$rFI$_9~$_$^$9!#(B

*** $(B%9%F%#%C%-!<%5%^%j(B($(B>C$($J$$%5%^%j(B)$(B$,3HD%$5$l$^$7$?!#(B
$(B%a%C%;!<%8%P%C%U%!$b%5%^%j$KBP1~$7$FMQ0U$5$l$k$h$&$K$J$j$^$7$?!#(B
$(B>o$K%9%F%#%C%-!<$K$J$k%U%)%k%@$r@_Dj$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(B$=$NB>(B

*** $(BJQ?t(B wl-draft-prepared-config-alist $(B$OGQ;_$5$l$^$7$?!#(B
wl-draft-config-alist $(B$KE}9g$5$l$^$7$?!#(B

*** POP-before-SMTP $(B4XO"$NJQ?t$,@0M}$5$l$^$7$?!#(B

*** $(BB8:_$7$J$$%U%)%k%@$r:n$k$+$I$&$+3NG'$9$k$h$&$K$J$j$^$7$?!#(B
 FCC: $(B$K?7$7$$%U%)%k%@L>$r;XDj$7$?$H$-$d!"(Bauto-refile $(B$G(B
 $(B?7$7$$%U%)%k%@L>$r;XDj$7$?$H$-$K%U%)%k%@$r:n$k$+$I$&$+3NG'$7$^$9!#(B

*** $(B%W%j%U%'%C%A$N3NG'$K4X$9$k@_Dj$NJQ?t$,2C$o$j$^$7$?!#(B
wl-prefetch-confirm-threshold, wl-cache-fetch-threshold.

*** $(B%U%)%k%@L>$N$"$@L>$r%U%)%k%@L>F~NO$GJd40$G$-$k$h$&$K$J$j$^$7$?!#(B

*** Message-ID $(B$N@8@.J}K!$,JQ$o$j$^$7$?!#(B

*** Mule $(B$G$O%S%C%H%^%C%W$N%*!<%W%K%s%0%G%b2hLL$,=P$k$h$&$K$J$j$^$7$?!#(B

*** `smtp-server' $(B$K4X?t$r;XDj$G$-$^$9!#(B

*** $(BAw?.%m%0$,J]B8$5$l$k$h$&$K$J$j$^$7$?!#(B
`wl-draft-sendlog' $(B$,(B non-nil $(B$N>l9g!"(B'sendlog' $(B%U%!%$%k$KJ]B8$5$l$^$9!#(B

*** $(B%*%U%i%$%s=hM}$G%W%j%U%'%C%A$rM=Ls$G$-$k$h$&$K$J$j$^$7$?!#(B

*** `wl-summary-incorporate-marks'

*** `wl-draft-use-frame' $(B$,(B non-nil $(B$J$i%U%l!<%`$r@8@.$7$^$9!#(B

*** $(B?75,JQ?t(B `wl-user-mail-address-list'$(B!#(B

*** $(B?75,JQ?t(B `wl-local-domain'$(B!#(B

*** IMAP4 $(B$G%5!<%PB&$NL$FI>uBV$r;2>H$9$k$h$&$K$J$j$^$7$?!#(B

*** $(B=i4|@_Dj$,JQ99$5$l$?JQ?t(B
  wl-mime-charset         iso-2022-jp  =>  x-ctext
  wl-summary-move-order   'new  =>  'unread
  wl-tmp-dir              TMPDIR  =>  ~/tmp/

*** $(B?75,(B hook
  wl-draft-send-hook
  wl-draft-reedit-hook
  wl-mime-edit-preview-message-hook
  wl-folder-suspend-hook
  wl-summary-toggle-disp-folder-message-resumed-hook
  wl-summary-line-inserted-hook
  wl-thread-update-children-number-hook
  mmelmo-header-inserted-hook
  mmelmo-entity-content-inserted-hook

*** $(B?75,%3%^%s%I(B
  wl-save
  wl-summary-write
  wl-summary-supersedes-message
  wl-fldmgr-delete
  wl-refile-guess-by-msgid
  wl-address-user-mail-address-p
  wl-summary-jump-to-msg-by-message-id-via-nntp
  wl-summary-temp-mark-pick
"))))

;;; -*- news-list-end -*-

(defun wl-news-previous-version-load ()
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  insert-file-contents-pre-hook
	  insert-file-contents-post-hook
	  ret-val)
      (if (not (file-readable-p filename))
	  (cons wl-news-default-previous-version
		wl-news-default-previous-version)
	(insert-file-contents filename)
	(condition-case nil
	    (read (current-buffer))
	  (error nil nil))))))

(defun wl-news-previous-version-save (current-version previous-version)
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  print-length print-level)
      (prin1 (cons current-version previous-version) (current-buffer))
      (princ "\n" (current-buffer))
      (if (file-writable-p filename)
	  (write-region (point-min) (point-max)
			filename nil 'no-msg)
	(message "%s is not writable." filename)))))

(defun wl-news-append-news (lang previous-version &optional no-mime-tag)
  (require 'wl-mime)
  (let ((news-list (cdr (assoc lang wl-news-news-alist)))
	ret)
    (when news-list
      (if no-mime-tag
	  (insert "--------------\n")
	(mime-edit-insert-tag "text" "plain" "" ""))
      (while (< 0
		(product-version-compare
		 (car (car news-list))
		 previous-version))
	(setq ret t)
	(insert (cdr (car news-list)) "\n\n")
	(setq news-list (cdr news-list))))
    ret))

(defun wl-news-check-news (version news-lang)
  (let ((lang news-lang)
	news-list ret)
    (while (car lang)
      (setq news-list (cdr (assoc (car lang) wl-news-news-alist)))
      (while (< 0
		(product-version-compare
		 (car (car news-list)) version))
	(setq ret t)
	(setq news-list (cdr news-list)))
      (setq lang (cdr lang)))
    ret))

(defun wl-news-already-current-p ()
  (>= 0 (product-version-compare
	 (product-version (product-find 'wl-version))
	 (car (wl-news-previous-version-load)))))

(defun wl-news-send-news (version news-lang folder)
  (require 'wl-draft)
  (let ((lang (if (listp wl-news-lang)
		  wl-news-lang
		(list wl-news-lang)))
	send-buffer
	wl-fcc wl-bcc ret)
    (save-window-excursion
      (set-buffer
       (setq send-buffer (wl-draft-create-buffer)))
      (wl-draft-create-contents
       (list (cons 'From "WL Release 'Bot <wl@lists.airs.net>")
	     (cons 'To (wl-draft-eword-encode-address-list wl-from))
	     (cons 'Subject "Wanderlust NEWS")
	     (cons 'Date (wl-make-date-string))
	     (cons 'User-Agent wl-generate-mailer-string-function)))
      (wl-draft-insert-mail-header-separator)
      (wl-draft-prepare-edit)
      (goto-char (point-max))
      (insert "\nThis message is automatically generated by Wanderlust.\n\n")
      ;; insert news
      (while (car lang)
	(wl-news-append-news (car lang) version)
	(setq lang (cdr lang)))
      ;; encode
      (let ((mime-header-encode-method-alist
	     '((eword-encode-unstructured-field-body))))
	(mime-edit-translate-buffer))
      (wl-draft-get-header-delimiter t)
      (setq ret
	    (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (elmo-folder-append-buffer
		  (wl-folder-get-elmo-folder folder))))
      (wl-draft-hide send-buffer)
      (wl-draft-delete send-buffer))
    ret))

;;; wl-news-mode

(defvar wl-news-buf-name "NEWS")
(defvar wl-news-mode-map nil)
(defvar wl-news-winconf nil)
(defvar wl-news-buffer-oldest-version nil)
(make-variable-buffer-local 'wl-news-buffer-oldest-version)

(unless wl-news-mode-map
  (setq wl-news-mode-map (make-sparse-keymap))
  (define-key wl-news-mode-map "q"     'wl-news-exit)
  (define-key wl-news-mode-map "Q"     'wl-news-force-exit)
  (define-key wl-news-mode-map "\C-xk" 'wl-news-exit)
  (define-key wl-news-mode-map "a"     'wl-news-show-all)
  (define-key wl-news-mode-map "m"     'wl-news-append-to-folder)
  (define-key wl-news-mode-map "\C-m"  'wl-news-next-line)
  (define-key wl-news-mode-map " "     'wl-news-next-page)
  (define-key wl-news-mode-map "\177"  'wl-news-previous-page)
  ;; re-bind commands of outline-mode
  (define-key wl-news-mode-map "n"     'outline-next-visible-heading)
  (define-key wl-news-mode-map "p"     'outline-previous-visible-heading)
  (define-key wl-news-mode-map "u"     'outline-up-heading)
  (define-key wl-news-mode-map "N"     'outline-forward-same-level)
  (define-key wl-news-mode-map "P"     'outline-backward-same-level))

(require 'derived)
(define-derived-mode wl-news-mode outline-mode "NEWS"
  "Mode for Wanderlust NEWS(.ja)."
  (setq buffer-read-only t))

(defun wl-news (&optional arg)
  (interactive "P")
  (remove-hook 'wl-hook 'wl-news)
  (let* ((previous-version (if arg wl-news-default-previous-version
			     (cdr (wl-news-previous-version-load))))
	 (lang wl-news-lang)
	 window-lines lines)
    (if (or (get-buffer wl-news-buf-name)
	    (if (wl-news-check-news previous-version wl-news-lang)
		(progn
		  (setq wl-news-winconf (current-window-configuration))
		  (set-buffer (get-buffer-create wl-news-buf-name))
		  (wl-news-mode)
		  (setq wl-news-buffer-oldest-version previous-version)
		  (buffer-disable-undo (current-buffer))
		  ;; insert news
		  (let ((buffer-read-only nil))
		    (insert "--- Wanderlust NEWS ---  press 'a' to show all NEWS\n")
		    (insert "                         press 'm' to mail this NEWS to your folder\n")
		    (insert "                         press 'q' to quit\n")
		    (insert "                         press 'Q' to force quit\n\n")
		    (while (car lang)
		      (wl-news-append-news
		       (car lang) previous-version t)
		      (setq lang (cdr lang))))
		  t)
	      (message "No NEWS.")
	      nil))
	(progn
	  (switch-to-buffer wl-news-buf-name)
	  (delete-other-windows)
	  (goto-char (point-min))))))

(defun wl-news-next-line ()
  (interactive)
  (scroll-up 1))

(defun wl-news-next-page ()
  (interactive)
  (scroll-up))

(defun wl-news-previous-page ()
  (interactive)
  (scroll-down))

(defun wl-news-show-all ()
  (interactive)
  (when (eq major-mode 'wl-news-mode)
    (kill-buffer (current-buffer))
    (wl-news t)))

(defun wl-news-exit ()
  (interactive)
  (let* ((oldest-version (cdr (wl-news-previous-version-load)))
	 (current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (buf (get-buffer wl-news-buf-name)))
    (when buf
      (if (wl-news-check-news oldest-version wl-news-lang)
	  (if (y-or-n-p "Do you want to see this message again? ")
	      (progn
		(message "Please M-x wl-news if you want to see it.")
		(setq new-old-version oldest-version))))
      (wl-news-previous-version-save
       current-version new-old-version)
      (kill-buffer (current-buffer))
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))

(defun wl-news-append-to-folder ()
  (interactive)
  (let* ((current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (folder wl-default-folder))
    (if (or (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (y-or-n-p (format
			    "Do you want to append this message to %s ? "
			    wl-default-folder)))
	    (setq folder
		  (wl-summary-read-folder wl-default-folder "to append ")))
	(or (wl-news-send-news wl-news-buffer-oldest-version wl-news-lang folder)
	    (error "Cannot append NEWS mail to %s" folder)))))

(defun wl-news-force-exit ()
  (interactive)
  (let ((buf))
    (when (setq buf (get-buffer wl-news-buf-name))
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load)))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))


(require 'product)
(product-provide (provide 'wl-news) (require 'wl-version))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; wl-news.el ends here

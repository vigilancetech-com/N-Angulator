;;; na.el -- N-Angulator: Persistent N-Dimensional Sparse Array, Editor, and Browser

;; Copyright (C) 2000
;;    Kevin Haddock and N-Angulator.org -- All Rights Reserved

;; Author: Kevin Haddock <support@n-angulator.org>
;; Maintainer: Kevin Haddock <support@n-angulator.org>
;; Keywords: database, hypermedia, persistent, object oriented, file tool, search engine
;; Version: 0.1
;; X-URL:

;; This program runs under XEmacs

;; N-Angulator is free software; you can redistribute it and/or modify it under the terms of
;; the N-Angulator.org Public License as published by N-Angulator.org
;; either version 1, or (at your option) any later version (see the file COPYING distributed
;; with this file or online at:
;;
;;    http://www.n-angulator.org/license


;; N-Angulator is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; N-Angulator.org Public License for more details.

;; You should have received a copy of the N-Angulator.org
;; Public License  along with N-Angulator; see the file COPYING.
;; If not, write to:
;;
;; N-Angulator.org
;; Attn: KEVIN HADDOCK
;; 975 East Ave. PMB 112
;; Chico, CA 95926, USA.

;;; Commentary:
;;
;; This program has a companion shell script `na.member.sh'.

;; This utility can optionally work in conjunction with an
;; automated document generation system (mrg.el) by the same author.

;; N-Angulator makes use of Unix-like file systems that allow hard links to files
;; (the same file showing up in different places in the file-system hierarchy under
;; possibly different names).  It turns this feature into an automated, multi-indexed,
;; multimedia file cabinet where items and groups of items can be incrementally searched
;; by doing a series of union and intersection 'set' operations.  In N-Angulator speak, files
;; are called 'leaves,' directories are called 'branches,' and the incremental searches called
;; 'angles.'  It is called 'N-angulator' because locating the desired information resembles
;; the process of 'triangulating' the source of a radio signal except that N different 'angles'
;; may be (and usually are) employed rather than just two.

;; N-Angulator also allows one to easily add, delete, and modify the elements in the tree, hence
;; it's role as file tool, or 'editor.'  It is 'persistent' because very rapidly (usually no
;; more than 30 seconds, depending on the operating system's cache) all changes are committed
;; to disk and more or less permanent.  It is somewhat object oriented because it uses the mime
;; library to deal with each file type independently.

;; N-Angangulator's possible (future) uses could be:  Internet bookmark file, search engine, 
;; link farm, sales contact management, multi-media archive, etc... or literally any extensive
;; multi-indexed filing application.

;; NOTE: N-Angulator uses a modified GPL license but construcively for contributors other
;; those specifically designated by its owner/originator, constibutions are considered governed
;; by GPL version 2.  Contact the owner/originator at support@n-angulator.org for further details.

;; To do:
;;   Get an invalid function nil menu when pulling off a branch/leaf menu.
;;   Na-pool-delete does not rebuild the pools correctly.  The subsequent branches
;;      don't have everything as members as if the deleted leaf had not been selected
;;      in the first instance.
;;   Try dynamic scoping of default-directory with creation as local then destruction reveals orig
;;   Add "display leaf type" and perhaps "display leaf info" to leaf menu.
;;   Add "display branch info" to branch menu.
;;   Somewhere along the line :pool gets a \n at its beginning which can cause excessive
;;      na-branch-arguments rebuilds
;;   Update screen widgets that have changed names or been deleted without disturbing anything else
;;   Stop Na from messing up parens display highlighting in elisp source
;;   Stop Na error clicking branch after creating leaf/link there
;;   Make Na use eshell functions wherever possible
;;   Fix pop up menu's solid lines (in widget package?)
;;   Make inactive widgets in popup menus gray and unselectable (in widget package?)
;;   Replicate this branch in new angle below.
;;   eradicate branch and it's contents
;;   Replicate this search in a new buffer.
;;   Think about ways that newly created buffers could interact (drag n drop, cut and paste?)
;;   Pseudo branches/leaves representing inode permissions, dates, #links, etc... information
;;   Make a log of changes (so other machines could reflect updates)
;;   Build an na.members.sh server (hashes for inode tables rather than 'find' shell cmd)
;;   Test/implement gnuserv/gnuclient to windows machines over the internet
;;   And/or/not radio buttons at the begining of each angle
;;   Implement sugrp as xemacs primitive
;;   Considering keeping directory inodes to improve screen updates
;;   Make leaf and branch modifications more object oriented
;;   Make Na look and behave somewhat like Nautilus (or integrate Na search features there)
;;   link to selection to already existing link pops bogus error
;;
;;; Code:

;(load "eshell-auto")
(require 'dired)
(require 'widget)
;(eval-when-compile (require 'cl)) ; for case function
(require 'mailcap)
;(require 'mm)
(require 'mm-view)
;(require 'mail-parse)
(require 'gnus)
(require 'edebug)
;(require 'mrg) ; comment out unless doing na-autolink

(mailcap-add-mailcap-entry
 "application" "x-empty" 
 '((viewer . fundamental-mode)
   (test   . (fboundp 'fundamental-mode))
   (type   . "application/x-empty")))

(mailcap-add-mailcap-entry
 "application" "dvi"
 '((viewer . "kdvi %s")
   (test   . (eq (mm-device-type) 'x))
   ("needsx11")
   (type   . "application/dvi")))



(setq na-leaves '((dummy :pool ""))) ; where the leaf widgets get pushed so we know if one has been
                                     ; selected (because menu behavior changes)
(setq na-angles nil) ; the base of the widget angles tree
(setq na-need-br-rebuild nil) ; where widgets that need branches rebuilt get's pushed

(defgroup na-documentation nil "")

(defcustom na-edit-mode t
  "True if desirous to edit leaf contents where-ever possible"
  :group 'na
  :type 'boolean)

(defcustom na-base-directory "/home/nadb/"
  "Root dir of na sparse array objects"
  :group 'na
  :type '(directory))

(defcustom na-control-directory (concat na-base-directory "/.control")
  "Root dir of na sparse array objects"
  :group 'na
  :type '(directory))

;(defcustom na-autolink-templates
;  "Default autolink templates file"
;  (concat na-control-directory ".autolink-templates")
;  :group 'na
;  :type '(file))

(defcustom na-root-tag "  /"
  "The default tag for each angle's root"
  ; Might want this to reflect which angle number later (e.g. a format?)
  :group 'na
  :type '(text))

(defcustom na-new-angle-tag "New Angle"
  "The tag that get's displayed in the menu for the root"
  :group 'na
  :type '(text))

(fset 'na-message 'message);'yes-or-no-p-dialog-box)

;;What is the theory of autolink?  There could be a file extension type; to look the files up in the
;;templates file; then in the templates file, the field, the base directory, then either a regular
;;expression or s-expression that gives the link or links for the autolink.  If the s-expression returns
;;more than one atomic string, then each string should be a link.  We need to set up a widget front end 
;;for editing the templates file so that a clerk person could do it.  How do we determine whether the
;;autolink contains a regular expression or a sexp?  Should there be a slot for each and run both
;;in sucession?  No, just an sexp because xemacs doesn't have a good regular expression function, only
;;string-match which just gives the starting address of the regexp in the string.
;;If a list, execute as an sexp, if a symbol, evaluate, (in either of these cases, if a list of strings
;;results, then link the source to each string destination), if a string, use it as the destination relative
;;to the root of the nadb and just concatenate the value of the symbol
;;So the format is:
;;   (template (symbol-name (sexp) symbol-name "person/banker" symbol-name 'parselink-csz)
;;    template (symbol-name "thing/precident/" symbol-name '(concat "event/continum/" (today)) etc...))

;; called internally only from na-autolink because it relys on run-time scoping of na-autolink's
;; local variables

;(defun na-autolink-atom (atom)
;  (setq result
;	(na-shell-string-read ; need to do a mkdir -p but I need to check to see if the file is there
;					; and a directory first; if so, then 
;	 (concat "ln \"" myname "\" \""
;		 (concat action
;			 (when (string-match "/$" action) ; trailing slash = symbolval=target
;			     value)) ; otherwise default buffer name or atom
;			   "\"; echo $?"))) ; any error handling?  figure out for whole system
;  result)

;;(defun na-autolink ()
;;  (let*((templates (load na-autolink-templates))
;;	(myname (buffer-file-name))
;;	(myext (cdr (split-string myname "^.+\\."))) ; extension of this template file
;;	(symbol (intern myext))		; create a symbol from the string to search for on the templates
;;	(template (get templates symbol)) ; find the template for this file extension
;;	value action result argument)
;;    (save-buffer)
;;    (mrg-set-symbols myname) ; create all the symbols for this template file (why do I pass filename here?)
;;    (while (car template)
;;      (setq value (eval (car template))) ; get the symbol's value from mrg-set-symbols operation above
;;      (setq (action (cadr template)))
;;      (if (stringp action) ; template is a string representing target file/directory
;;	  (na-autolink-atom action)
;;	(if (listp action) ; sexp
;;	    (setq result (or (mapcar 'na-autolink-atom (eval action))))
;;	  (funcall action))) ; or symbol
;;      (setq template (cddr template))))) ; symbol or sexp = eval it

;; this would be real nice if it linked the default buffer name into the destination.  Will it work
;; if there is no slash following?  Yes that will work but it needs to be *well* documented
(defun na-date (base)
  "Return string representing link for format year/month/day"
  (let ((list (split-string value "[, ]+")))
    (concat base 
    (mapconcat 'identity
	       (list (third list) (first list) (second list)) "/"))))

(defun na-csz (base)
  "Return string representing link for formats '(zip state/city)"
  )

(defun na-rebuild-brmenus (&optional candidate)

"Rebuild the given widget's branch menu or all the menus flagged as
helneeding rebuilding by na-branch-delete if none given.  In either case,
remove the rebuilt menu(s) from the list of ones flagged.

This is factored out because when moving links, first you start with a delete,
then a create and sometimes those may effect the same branch, however that branch
menu only needs to be rebuilt once.  Not a common occurrance but possible."
  (let (widget)
    (if candidate
	(progn
	  (when (memq candidate na-need-br-rebuild)
	    (setq na-need-br-rebuild
		  (delq candidate na-need-br-rebuild)))
	  (widget-put candidate :args (na-branch-arguments candidate)))
      (while (setq widget (pop na-need-br-rebuild))
	(widget-put widget :args (na-branch-arguments widget))))))

;;
;; DEBUGGING NOTE:
;;
;; The following two functions may cause a bug in that they may put directory
;; inodes into the pool (which has not been done so far).  This may become necessary
;; in order to implement redisplay when a directory name has changed (since without the
;; directory's inode, we have no way of telling what it's name was.
;; Either I should make accomodations to have directory inodes be in all pools, or
;; I should probably strip them out here (with a long ls listing and a grep -v '^d' on
;; the output or by using find to only send to ls the regular file entries)
;;
;; after deletion, it appears to lose the fact that it already has a leaf selected
;; so the branch menus ask to create a new leaf rather than add a link.
;;
;; Also, it makes the first leaf display a branch navigation menu.
;;
;; A thought: Set the child's grandchild to the (na-lastnode)'s child so that
;; when the deletion comes down we have something that looks semi-normal
;; for it to mess with.
;
; Should probably do widget-setup after this and na-rebuild-brmenus
(defun na-pool-delete (inode &optional nextwidget inodeorig)
  "remove inode(s) from nextwidget and its parents if that file does not
exist in their directories.

If nextwidget is not given, the deletion starts from the last widget
in the angles stack: (na-lastnode)"
  (let* ((nextwidget (or nextwidget (na-lastnode))) ;if arg not given, start at end of stack
	 (inodeorig (or inodeorig inode)) ; preserve the original inode list if first entering
	 (parent (widget-get nextwidget :parent))
	 (child (car (widget-get nextwidget :children)))
	 (cname (and child (widget-get child :tag)))
	 default-directory wpoolorg wpool regexp apool children buttons leaves)
; moving the following statment up nearer the top of this function would make it quicker
    (if (eq (car nextwidget) 'leaf)	;skip leaves
	(na-pool-delete inode parent inodeorig)
      (setq default-directory (widget-get nextwidget :path) ; set it to nextwidget's
	    wpoolorg (widget-get nextwidget :pool) ; get the widget's original pool
	    apool
	    (na-shell-read		; pool of inodes actually in this directory
	     "echo -n \\\";find * -maxdepth 0 -type f -printf '%i\n' 2>/dev/null |sort|grep -v '^$'|uniq; echo \\\"")
	    inode (na-isubtract apool inode))
;;; handle deletion before flagging others for branch menu rebuilding
;;; so leaves don't get flagged for menu rebuilding. -- still doesn't work
      (when (and cname
		 (not (equal cname na-root-tag))
		 (not (file-exists-p cname))) ; delete widget from screen
	(setq children (widget-get child :children)
	      buttons (widget-get child :buttons)) ; preserve grandchildren
	(widget-put child :children nil) ; blast link to them
	(widget-put child :buttons nil)
;	(widget-put child :parent nil)  why this?
	(widget-apply nextwidget :value-delete)  ; delete child from screen
;	(funcall (na-default 'branch :value-delete) nextwidget)
	;; now remove all deleted inodes from na-leaves
;	(setq leaves na-leaves)
;	(while leaves
;	  (when ; child's name and path is same as head of na-leaves
;	      (and (equal cname (widget-get (car leaves) :tag))
;		   (equal (widget-get child :path) (widget-get (car leaves) :path)))
;	    (setq na-leaves (delq (car leaves) na-leaves))) ; remove it from na-leaves
;	  (setq leaves (cdr leaves))) ; step through
	(widget-put nextwidget :children children) ; link around child
	(widget-put nextwidget :buttons buttons)
	(widget-put (car children) :parent nextwidget)
;	(funcall (widget-get nextwidget :value-set) nextwidget child)
	(pushnew nextwidget na-need-br-rebuild)) ; flag this one for menu rebuild
      (unless (eq 0  ; any branch containing this inode needs menu rebuilding
		  (length ; because it could have existed more than once here
		   (na-iinboth apool inodeorig)))
	(pushnew nextwidget na-need-br-rebuild))
      (when (string-match ".+" inode)		; if there are any left to delete
	(setq wpool (na-subtract wpoolorg inode)); eliminate from widget's pool ones actually found in this directory
	(widget-put nextwidget :pool wpool)
	(unless (eq (length wpool) (length wpoolorg)); if change, flag parent rebuid branch menu
	  (pushnew nextwidget na-need-br-rebuild)))
      (when parent;  not past top of 1st angle
	(na-pool-delete inode parent inodeorig)))))



(defun na-pool-add (inode &optional nextwidget)
  "add inode(s) to nextwidget and its children if that/those file
exists (i.e. have been added) in their directories.  Also update
branch menus for any branch wherein inode(s) is(are) found.

If nextwidget is not given, the addition starts from the first widget
in the angles stack: na-angles"
; this is a convoluted mess and needs to be refactored, but hey, it works, mostly :-)
  (catch 'done
    (let* ((nextwidget (or nextwidget na-angles)) ;if arg not given, start at beginning of stack
					; pool of inodes actually in widget's directory
	   args ; steps through args to find child's new pool and set it
	   default-directory		;preserve current directory
	   (child (car (widget-get nextwidget :children))) ; set directory to this widgets
	   cname wpool apool dpool tmp)
;      (when (eq (car child) 'leaf)	; skip leaves
      (when (eq (car nextwidget) 'leaf)
	(na-pool-add inode child)
	(throw 'done nil))
      (setq cname (widget-get child :tag)
	    wpool (widget-get nextwidget :pool)
	    default-directory (widget-get nextwidget :path)
	    apool (na-shell-read		; pool of inodes actually in this directory
		   "echo -n \\\";find * -maxdepth 0 -type f -printf '%i\n' 2>/dev/null|sort|grep -v '^$'|uniq; echo \\\"")
	    dpool (na-shell-read	; pool of inodes representing directories
		   "echo -n \\\";find * -maxdepth 0 -type d -printf '%i\n' 2>/dev/null|sort|grep -v '^$'|uniq; echo \\\""))
      (when (eq nextwidget na-angles) ; no nextwidget argument was given (i.e. first time through)
	(setq tmp  ;tmp=inode w/directory ones removed
	      (na-isubtract inode dpool))
	(widget-put nextwidget :pool
		    (setq wpool (na-iadd inode wpool))))
;      (edebug)
      (when (string-match		; if some of the inodes still in nextwidget's pool
	   ".+" (na-iinboth inode wpool))  ; means they have not been made "others" by elimination yet
	    (na-rebuild-brmenus nextwidget)	;rebuild it's branch menu (and remove from rebuilds list)
	    (setq args (widget-get nextwidget :args))
	    (if (widget-get child :path) ;nextwidget has children and they are "real"
					;figure out which argument is child and set child's pool to that
					;  argument's pool (thats just been updated with na-branch-arguments)
		(while args ; some day need to see if setting child's pool to :choice's pool would work
		  (when (or (equal (widget-get (car args) :tag) cname) ; to speed this up (eliminate looping)
			    (and (equal cname na-root-tag)
				 (equal (widget-get (car args) :tag) na-new-angle-tag)))
		    (widget-put child :pool (widget-get (car args) :pool))
		    (setq args nil))
		  (setq args (cdr args)))
	      (throw 'done nil))) ; otherwise bail out
;	(edebug)
	(when (or
	       (string-match   ; some inodes exist in current directory
		".+" (na-iinboth apool inode))
					; this widget on list of ones needing rebuild
	       (memq nextwidget na-need-br-rebuild))
	  (na-rebuild-brmenus nextwidget))
      (unless (widget-get child :path) ; bail if no actual child
	(throw 'done nil))
;      (edebug)
      (na-pool-add inode child)))) ; recurse in on 'child' (next node in stack)

    
(defsubst na-default (type keyword)
  "Get the value of the keyword from the type this widget is derived from, e.g. the 'default'
value if this widget did not override the parent"
  (widget-get (get (widget-type (get type 'widget-type)) 'widget-type) keyword))


(defun na-mime-method (name)
  (mailcap-mime-info
   (car
    (mail-header-parse-content-type
     (na-shell-string-read
      (format "%s; name=\"%s\""
	      (format "file -i \"%s\" 2> /dev/null | cut -d ':' -f2- | tr -d ' \n'" name) name))))))

(defun na-mime-handle (name)
  (mail-header-parse-content-type
   (format "%s; name=\"%s\""
	   (na-shell-string-read
	    (format "file -i \"%s\" 2> /dev/null | cut -d ':' -f2- | tr -d ' \n'" name))
	   name)))

(define-widget 'leaf-edit 'choice-item ; was 'leaf
  "The widget that actually edits the file"
)

(define-widget 'leaf 'menu-choice
  "the widget representing the actual file; the 'terminal node'"
  :format "%[%t%]%v"
  :void '(item :format "")
  :mouse-down-action 'na-leaf-press-action
  :value-create 'na-leaf-create
  :value-delete 'na-leaf-delete
  :create 'na-node-create

;   Need to pop up a menu of available applications if there are more
;   than one acceptable one to display/edit a file, or have an edit v. view mode
;   and preferred applications for each (like the first one in .mailcap be the viewer
;   and if there is another one, have it be the default editor).

  :value-set (lambda (widget value)
	       (if (eq  (widget-type (widget-get widget :explicit-choice))
			'leaf-edit)
		   (let ((name (widget-get widget :tag)))

		     (setq foo (start-process "view" "*scratch*" "xdg-open"  name )))
;		     (eshell-command (concat "xdg-open " "\"" name "\" &") "*scratch*"))
			 ;There appears to be junk left in ~/tmp -- needs to be del'd
		     (funcall (na-default 'leaf :value-set) widget value))))

; this was the old code above
;		   (save-excursion
;		     (let* ((name (widget-get widget :tag))
;			    (buffer
;			     (generate-new-buffer
;			      (symbol-name (gensym))))
;			    (handle (list buffer  (na-mime-handle name)
;					  `(filename . ,name)
;					  nil nil nil '())))
;		       (if (and (mm-inlinable-p handle)
;				(mm-inlined-p handle))
;			   (progn
;			     (kill-buffer buffer)
;			     (setq buffer (find-file-noselect name))
;			     (goto-char (point-min buffer) buffer)
;			     (switch-to-buffer buffer))
;; 			 (set-buffer buffer); the gensym'ed one.
;; 			 ;; if coding system is not set, any file
;; 			 ;; with the wrong, or no, file extension
;; 			 ;; will not work with external viewers
;; 			 ;; because it gets altered when bringing
;; 			 ;; it into the buffer and a corrupted
;; 			 ;; file is output by subfunctions of
;; 			 ;; mm-display-part below
;; 			 (setq  buffer-file-coding-system-for-read
;; 				mm-binary-coding-system) ;

;; 			 ; read the file in there
;; 			 (if (not (eq (car (cdr (insert-file-contents name))) 0))
;; 			     (mm-display-part handle)
;; 			   (find-file name))
;			 (kill-buffer buffer)


(defun na-leaf-delete (widget)
  "Remove widget from the na-leaves before completing the delete"
  (message "in na-leaf-delete")
  (funcall (na-default 'leaf :value-delete) widget)
  ;; now remove all deleted inodes from na-leaves
  (let ((leaves na-leaves))
    (while leaves
      (when ; widget's name and path is same as head of na-leaves
	  (and (equal (widget-get widget :tag) (widget-get (car leaves) :tag))
	       (equal (widget-get widget :path) (widget-get (car leaves) :path)))
	(setq na-leaves (delq (car leaves) na-leaves))) ; remove it from na-leaves
      (setq leaves (cdr leaves))))) ; step through

;  (while (not (eq widget (pop na-leaves))) ())) ; this needs to change to accomodate new na-pool-delete
                                                ;   and screen updating.

(defun na-leaf-create (widget)
  "Needed to be modified slightly to make sure the path is set correctly"
  (let* ((tag (widget-get widget :tag))
	 (pool (na-inodes tag)))
    (widget-put widget :pool pool) ; this has to be intelligent.  If the pool
    ;; would have had the leaf in it, then it is made a member, otherwise
    ;; it is an other.  This presents a bit of a problem, because if you just
    ;; created a leaf, you almost assuredly would want to select it, wouldn't
    ;; you?  It would make no sense, though, to add it to the pool if that
    ;; were impossible (e.g. prior angles exclude it).
    ;; How do we tell if it is possible for the new leaf to be a member?  How
    ;; about if there is another angle above the branch where this leaf is
    ;; being created?  That would pretty much cinch it.  How do I test for
    ;; that?
    ;; Should not allow creation of leaves in any but the first angle.  Then
    ;; blow away all entries after the subbranch wherein leaf was created.
    ;; better yet: only show create-leaf menu item when there are items in 'members?'
    ;; that way it can be made a member when created?
    ;; is that right?
    ;; leaf creation rules:
    ;; - leaf will go into members if on first angle or all prior angles are subangels of this
    ;; - otherwise leaf will go into others
    ;;   (how to test for this?)
    ;;   (should a leaf be able to be created in others?  I think so because otherwise
    ;;    how do you create the fist leaf in the system?)
    ;;   (also, 
    (widget-put widget :path default-directory) ; buggy line?
    (push widget na-leaves)
    (widget-put widget :args
		(list
		 (widget-convert `(angle
				   :tag "New Angle"
				   :pool ,pool
				   :args ,(na-branch-arguments widget)))
		 (widget-convert `(leaf-edit
				   :tag ,tag))))
    (funcall (na-default 'leaf :value-create) widget))) ; during value-create

(defun na-leaf-press-action (widget &optional event)
  "The function that gets called when you press on a leaf"
  (setq na-location widget)
  (let ((button (event-button event))
	(selected (widget-value na-selection)))
    (setq default-directory (widget-get widget :path)
	  na-location widget)
    (case button
      (2
	 (funcall (widget-choose
		   "Leaf Command Menu"  
		   (append
		    '(("Remove this Link" . na-remove-link))
		    '(("Remove all Occurances" . na-purge-leaf))
		    '(("Set as Selection" . na-set-selection))
		    '(("Rename Leaf" . na-rename-leaf))
		    '(("Edit All Angles" . na-all-angles))
		    '(("Display all Angles" . na-all-angles-display))
		    '(("Demote Node" . na-demote-node))
		    (unless (equal "" selected)
		      (if (string-match "/$" selected)
			  (list
			   '("Link to Selection" . na-link-select)
			   '("Copy to Selection" . na-copy-select)
			   '("Move to Selection" . na-move-select))
			'(("Duplicate Links to Selected" . na-dup-links)))))
		   event)))
       (3
	(funcall (na-default 'leaf :mouse-down-action) widget event)))))

(defun na-dup-links ()
  "Link current leaf to all places selected leaf is linked."
  ; first get a list of all the branchnames selected has
;  (setq savedir default-directory          ;; debugging stuff
;	selected (concat na-base-directory (widget-value selection))
;	existing (concat (widget-get widget :path)
;			 (widget-get widget :tag)))
  (let ((default-directory "/") ; relying on dynamic scoping -- is this OK?
	(selected (widget-value na-selection))
	(existing (concat (widget-get widget :path)
			  (widget-get widget :tag))))
    (na-error
     (na-shell-string-read
	    (mapconcat ; map a link of the existing file to all the directories that selected exists in
	     (lambda (x)
	       (format
		"ln \"%s\" \"%s\""
		existing (concat na-base-directory
				 (substring x 0 (string-match "[^/]+$" x)))))
	     (let ((default-directory na-base-directory)) ; create a local default-directory.
					; depend on dynamic scoping so na-shell-read
	                                ; will get it rather than the global version
	       (na-shell-read
		(format
		 "echo -n \\(; find . -inum %i -printf '\"%%p\" '; echo \\)"
		 (string-to-number
		  (na-inodes (substring selected 1)))))) ";" )))))

(defun na-demote-node ()
  "Turn this leaf into a branch with itself as its first contents"
  (let* ((name (widget-get widget :tag))
	 (newname (read-from-minibuffer "Enter new link name: "
					(widget-get widget :tag)))
	 (split (split-string newname "/"))
	 (node (nth (1- (length split)) split))) ; what was I thinking here?
	 (unless (eq "" newname)
	   (na-error
		 (na-shell-string-read
		  (concat
		   "tmpfile=na.demoted.$$;"
		   "mv \"" 
		   name
		   "\" $tmpfile &&"
		   "mkdir \"" name
		   "\" 2>/dev/null&&"
		   "mv $tmpfile \"" name "\"/\"" newname "\"")))
	   (na-refresh-screen widget))))

(defun na-rename-leaf ()
  "Rename the selected leaf"
  (let* ((name (widget-get widget :tag))
	 (newname (read-from-minibuffer "Enter new link name: "
					(widget-get widget :tag)))
	 (split (split-string newname "/"))
	 (node (nth (1- (length split)) split))) ;; what was I thinking here?
	 (unless (eq "" newname)
	   (na-error
		 (na-shell-string-read
		  (concat "mv \""  name "\" \""
			  (when (eq 0 (string-match "/" newname))
			    na-base-directory)  newname
			    "\"")))
	   (na-refresh-screen widget))))

; the destination menu/pool does not get updated here
; probably because the destination is not the current
; widget, so it somehow needs to get set to that
; then the 'current' widget updated
; should be the same for all these 'select' functions
(defun na-link-select ()
  "Link the selected leaf to the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
	 (branchnode (nth (1- (length branchsplit)) branchsplit))
	 (newname (read-from-minibuffer "Enter new link name: " name)))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "ln \""  name "\" \""
			  (concat na-base-directory branchname
			  newname "\""))))
	   (na-pool-add branchnode)
	   (na-rebuild-brmenus)
	   (na-update-all-named branchnode)
	   (na-update-all-named branchname))))
  
(defun na-copy-select ()
  "Copy the selected leaf to the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
	 (branchnode (nth (1- (length branchsplit)) branchsplit))
	 (newname (read-from-minibuffer "Enter new leaf name: "
					(widget-get widget :tag))))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "cp \""  name "\" \""
			  na-base-directory branchname
			  newname "\"")))
	   (na-pool-add branchnode)
	   (na-rebuild-brmenus)
	   (na-update-all-named  branchnode))))
  
(defun na-move-select ()
  "move the selected leaf to the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
	 (branchnode (nth (1- (length branchsplit)) branchsplit))
	 (newname (read-from-minibuffer "Enter new leaf name: " name)))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "mv \""  name "\" \""
			  na-base-directory branchname
			  newname "\"")))
	   (na-pool-add branchnode)
	   (na-rebuild-brmenus)
	   (na-update-all-named  branchnode))))
  

(defun na-purge-leaf ()
  "Remove the selected leaf entirely"
  (let* (temp
	 (name (widget-get widget :tag))
	 (inode (widget-get widget :pool))
	 (parent (widget-get widget :parent))
	 (parents (mapcar (lambda (x)	;get list of parents tag names
			    (file-name-as-directory
			     (file-name-nondirectory
			      (substring x 1 (1- (length x))))))
			  (mapcar 'file-name-directory (na-allnames-unsplit inode)))))
    (when (yes-or-no-p-dialog-box
	   (format "Purge %s entirely?" name))
      (setq temp default-directory	;(delete-file) works relative to default-directory
	    default-directory na-base-directory) ;however we are handing files relative to
      (mapcar 'delete-file (na-allnames-unsplit inode))	; na-base-directory
      (setq default-directory temp)	;so we preserve it in temp
      (funcall (widget-get parent :value-delete) parent) 
      (na-pool-delete inode)
      (na-rebuild-brmenus)
      (na-update-all-named parents))))

(defun na-remove-link ()
  "Delete the selected link"
  (let* ((name (widget-get widget :tag))
	 (inode (widget-get widget :pool))
	 (parent (widget-get widget :parent))
	 (parent-name (widget-get parent :tag)))
    (when (yes-or-no-p-dialog-box (format "Delete link %s?" name))
      (na-error (na-shell-string-read
		    (concat "rm -f \"" name "\"")))
;	    (funcall (widget-get parent :value-delete) parent)
	    (na-pool-delete inode)
	    (na-rebuild-brmenus)
	    (widget-setup))))

(defun na-leaf-release-action (widget &optional event)
  "What get's called when you release the button on a leaf"
    (setq default-directory (widget-get widget :path))
      (funcall (na-default 'leaf :action) widget event))


(defun na-all-angles ()
  "Edit all angles for this widget's inode"
  (let ((inode (na-inodes (widget-get widget :tag)))
	(pool (widget-get na-angles :pool))
	(leaves na-leaves) ; save a copy of leaves stack
	newangles leaves2)
    (goto-char (point-max))
    (setq na-leaves '((dummy :pool ""))) ; reset stack to empty
    (setq newangles
	  (widget-create
	   (na-all-angles-list
	    (na-allnames inode) na-base-directory
	    (na-inodes na-base-directory)))
	  leaves2 na-leaves ; save the new leaves stack
	  na-leaves leaves) ; restore the old one for the upcoming delete
    (widget-delete na-angles)
    (setq na-angles newangles
	  na-leaves leaves2)
    (widget-setup)))

(defun na-all-angles-display ()
  "Show a read only popup dialog displaying all angles for this widget's inode"
  (let ((inode (widget-get (car na-leaves) :pool)))
    (make-dialog-box
     'question :modal nil :title "Display All Angles"
     :question
     (na-shell-string-read
      (format "(cd \"%s\";find * -inum %s -printf \"/%%p\n\n\" 2>/dev/null)"
	      na-base-directory (substring inode 0 (1- (length inode)))))
     :buttons '(["Dismiss" '() t]))))


; a directory is just a node (choice-item) when it is a part
; of a menu (of a parent)
(define-widget 'branch 'menu-choice
  "A widget representing basically, a directory"
  :format "%[%t%]%v"
;  :validate (lambda (x) t)
  :validate (lambda (x) ; invalid to select an 'other' leaf
;	      (edebug)
		(if (eq (widget-type x) 'node)
		    (let (inode)
		      (or
		       (equal ""
			      (setq inode    ; a leaf inode exists?
				    (widget-get 
				     (car na-leaves) :pool)))
		       (equal inode (widget-get x :pool)))) ; pool = inode
		  t))
  :mouse-down-action 'na-branch-press-action
  :action 'na-branch-action
  :void '(item :format ""))


;;  truth table for following function (xor)
;; eventp  button!=3 action
;; -----------------------
;;   0       0        1
;;   0       1        1
;;   1       0        1
;;   1       1        0

  
(defun na-branch-action (widget &optional event)
  "The function that get's called when you click on a branch"
  (when (or (not (eventp event)) (eq (event-button event) 3))
      (funcall (na-default 'branch :action) widget event)))


(defun na-branch-press-action (widget &optional event)
  "What get's called when you press on a branch"
  (let ((button (event-button event))
	(path (widget-get widget :path)))
    (setq default-directory (or path default-directory)
	  na-location widget)
    (case button
      (2
	 (funcall (widget-choose
		   "Branch Command Menu" 
		   (append
		    '(("Rename Branch" . na-rename-branch)) ; unless a root?
		     (if (cdr na-leaves) ; any actively selected leaves?
			 '(("Add a Link" . na-add-link))
		       '(("Create New Leaf" . na-new-leaf)
			 ("Create BookMark" . na-bookmark)))
		     '(("Set as Selection" . na-set-selection))
		     '(("Add New Subbranch" . na-add-branch))
		     '(("Prune Branch" . na-prune-branch))
		     (unless (equal "" (widget-value na-selection))
		       (list
			'("Link Contents to Selection" . na-branch-link)
			'("Copy Contents to Selection" . na-branch-copy)
			'("Move Contents to Selection" . na-branch-move))))
		   event)))
      (3
       (funcall (na-default 'branch :mouse-down-action) widget event)))))

(defun na-branch-move ()
  "Move this branch and all the contents into the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
	 (branchnode (nth (1- (length branchsplit)) branchsplit)) ; variable not used?
	 (newname (read-from-minibuffer "Enter new branch name: " name)))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "mv . \"" na-base-directory branchname newname 
			  "\""))))
	       (na-refresh-screen widget)))


(defun na-branch-copy ()
  "Link all the contents of this branch into the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
;	 (branchnode (nth (1- (length branchsplit)) branchsplit))
	 (newname (read-from-minibuffer "Enter new branch name: " name)))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "find . -print | cpio -padm --quiet \"" 
			  na-base-directory branchname  newname
			  "\"")))
	       (na-refresh-screen widget))))

(defun na-branch-link ()
  "Link all the contents of this branch into the selection"
  (let* ((name (widget-get widget :tag))
	 (branchname (widget-value na-selection))
	 (branchsplit (split-string branchname "/"))
	 (branchnode (nth (- (length branchsplit) 2) branchsplit))
	 (newname (read-from-minibuffer "Enter new branch name: " name)))
	 (unless (eq "" branchname)
	   (na-error
		 (na-shell-string-read
		  (concat "find . -print | cpio -padlm --quiet \"" 
			  na-base-directory branchname  newname
			  "\"")))
	   (na-update-all-named (concat branchnode "/")))))



(defun na-rename-branch ()
  "This gets called from a branch menu to rename the branch"
  (let* ((tag (widget-get widget :tag))
	 (newname (read-from-minibuffer (format "Rename %s to: " tag tag)))
	 (result (when newname
		   (na-shell-string-read
		    (concat "cd ..;mv \"" tag "\" \""
			     newname "\"")))))
    (na-error result)
	(na-refresh-screen widget)))


(defun na-set-selection ()
  "Set this branch as the selection for future copys or moves"
  (widget-value-set
   na-selection
   (na-path na-location))
  (widget-setup))

(defun na-path (widget)
  "Return the string that represents the path to the given widget"
  (if (eq 'root (widget-type widget))
      "/"
    (concat
     (na-path
      (widget-get widget :parent))
      (widget-get widget :tag))))

(defun na-add-branch ()
  "Create a new sub-branch under the current branch"
  (let* ((name (read-from-minibuffer "Enter new sub-branch name: "))
	(result
	 (na-shell-string-read
	  (concat "mkdir \"" name "\";"))))
    (na-error result)
    (na-update-all-named (widget-get widget :tag))
    (widget-put widget :value (concat name "/"))))

(defun na-prune-branch ()
  "Purge this branch and its subnodes"
  (let ((name (widget-get widget :tag))
	(parent (widget-get widget :parent))
	(inode (widget-get widget :pool))
	result)
    (when (yes-or-no-p-dialog-box
	   (format "Delete branch %s and its contents?" name))
      (na-error (na-shell-string-read
		    (concat "(cd ..; rm -fr \"" name
			    "\")")))
      (funcall (widget-get parent :value-delete) parent)
      (setq default-directory (widget-get parent :path))
      (na-pool-delete (widget-get widget :pool))
      (na-rebuild-brmenus)
      (na-update-all-named
       (widget-get parent :tag)))))

; this function originally had the following line right after 'echo' below
;<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
(defun na-bookmark ()
  "Create a new bookmark under the current branch"
  (let* ((newname (read-from-minibuffer "Enter bookmark name: "))
	 (newurl (read-from-minibuffer "Enter URL: " "http://www."))
	 (result (when newname
		   (na-shell-string-read
		    (concat "test ! -e \"" newname
			    "\" && echo '<html>
<meta http-equiv=\"REFRESH\" content=\"0;url="
newurl "\"><html>'  > \""
			     newname
			    "\" || echo \"Error: File \'\"" newname
			    "\"\' exists\! \""))))
	(tmp widget)
	(inode (na-inodes newname)))
;    (while tmp
;      (widget-put tmp :pool		;don't want to add to pool here if should be other
;		(exec-to-string
;		 (format "echo '%s' | sort -u"
;			 (concat  (widget-get tmp :pool)
;			inode))))
;      (setq tmp (widget-get tmp :parent)))
    (na-error result)
    (na-pool-add inode)
    (na-rebuild-brmenus)
    (widget-setup)))

(defun na-new-leaf ()
  "Create a new leaf under the current branch"
  (let* ((newname (read-from-minibuffer "Enter new leaf name: "))
	 (result (when newname
		   (na-shell-string-read
		    (concat "test ! -e \"" newname "\" && > \""
			     newname
			    "\" || echo \"Error: File '\"" newname
			    "\"' exists\! \""))))
	(tmp widget)
	(inode (na-inodes newname)))
;    (while tmp
;      (widget-put tmp :pool		;don't want to add to pool here if should be other
;		(exec-to-string
;		 (format "echo '%s' | sort -u"
;			 (concat  (widget-get tmp :pool)
;			inode))))
;      (setq tmp (widget-get tmp :parent)))
    (na-error result)
    (na-pool-add inode)
    (na-rebuild-brmenus)
    (widget-setup)))
       	     
(defun na-add-link ()
  "Create a new link for the given widget leaves (should all be same inode)"
  (let* ((leaf (car na-leaves))
	 (path (widget-get leaf :path))
	 (name (widget-get leaf :tag))
	 (branchname (widget-get widget :tag))
	 (newname (read-from-minibuffer "Enter new link name: "
					 (widget-get leaf :tag)))
	 (result
	  (na-shell-string-read
	   (concat "ln \"" (concat path name) "\" \""
		   newname "\"")))
	 (tmp widget)
	 (inode (na-inodes newname)))
;    (while tmp
;      (widget-put tmp :pool
;		  (exec-to-string
;		   (format "echo '%s' | sort -u"
;			   (concat (widget-get tmp :pool)
;				   inode))))
;      (setq tmp (widget-get tmp :parent)))

    (na-error result)
    (na-pool-add inode)
    (na-rebuild-brmenus)
    (widget-setup))) ; new link will only show up on branch menus so
                                       ; this is the proper refresh


(defun na-refresh-screen (widget &optional event)
  "update all node's menus stopping at any change"
  (goto-char (point-max)) ; put new tree at end
  (let* ((source na-angles)
	 (newangles
	  (apply 'widget-create 'angle :path na-base-directory
		 :pool (na-inodes na-base-directory)
		 '((item ""))))
	 (select  newangles)
	 (sourcechild (car (widget-get source :children)))
	 (leaves na-leaves) ; save original na-leaves
	 leaves2 path type pool tag choice args)
    (setq na-leaves '((dummy :pool ""))) ; reinitialize na-leaves
    (catch 'changed
      (while (car (widget-get sourcechild :children))
	(setq default-directory (setq path (widget-get select :path))
	      pool (widget-get select :pool); Isn't this auto-generated somewhere?
	      type (widget-type sourcechild)
	      tag (widget-get sourcechild :tag)
	      tag (if (equal tag na-root-tag)
		      na-new-angle-tag
		    tag))
	(unless (equal tag na-new-angle-tag)
	     (unless
		 (directory-files ; file/directory still exists
		  path
		  nil ; not full names
		  (concat "^"
			  (car (split-string tag "/"))
			  "$") ; match regexp
		  t ; nosort
		  nil)
	     (throw 'changed t)))
	(setq choice
	      (let ((args (widget-get source :args))
		    current found)
		(while (and args (not found))
			    (setq current (car args)
				  args (cdr args)
				  found
				  (equal tag
					 (widget-get
					  current :tag))))
		current))
	(widget-put select :explicit-choice choice)
	(widget-put select :value tag)
	(widget-value-set select tag)
	(setq source (car (widget-get source :children))
	      sourcechild (car (widget-get source :children))
	      select (car (widget-get select :children)))))
    (setq leaves2 na-leaves
	  na-leaves leaves)
    (widget-delete na-angles)
    (setq na-leaves leaves2)
    (setq na-angles newangles)
    (widget-setup)))

(defun na-update-all-named (name)
  "update all the widget's menus where the widgets are tagged with 'name'"
  (let ((child na-angles)  ;handles either a string naming the parent directory, or a list of strings
	(names (if (stringp name) name
		(apply 'concat name)))
	cname)
    (while (setq cname (widget-get child :tag))
      (when (string-match cname names) ; when a value is getting deleted, it should be deleted here 
					; as well to update the display properly.
	(widget-put child :args (na-branch-arguments child)))
      (setq child (car (widget-get child :children))))))

(defsubst na-do-args (dirs files) 
  "Return a list of the node and leaf arguments from the given list of names
and inode strings"
  (let (inode)
    (append
     (mapcar
      (lambda (x)
	`(node :tag ,(concat (first x) "/")
	       :path ,(concat default-directory (first x) "/")
	       :value ,(first x)
	       :pool ,(second x)
	       :args ((choice-item ""))))
      dirs)
     (mapcar (lambda (x)
	       `(leaf :tag ,(first x)
		      :path ,default-directory
		      :value ,(first x)
		      :pool ,(second x)
		      :inactive ,(equal (second x) "")
		      :args ((angle
			      :tag "New Angle"
			      :format "\n\n %[%t%]%v" ; optional?
			      :path ,na-base-directory
			      :pool ,(second x)
			      :value "New Angle"
			      :args ((choice-item ""))))
		      :args ((choice-item ""))))
	     files))))

(defsubst na-branch-arguments (widget)
  "Return a properly formatted :arg list for the given widget"
  (let* ((pool (widget-get widget :pool))
	 (foo (setq default-directory (widget-get widget :path)))
	 (data (na-shell-read
		(concat "echo -n \"" pool "\" | nd.members.sh"))))
    (append
     `((angle :tag "New Angle"
	      :format "\n\n %[%t%]%v"
	      :path ,na-base-directory 
	      :pool ,pool
	      :value "New Angle"))
	    ;  (choice-item "")))
     '((choice-item :value "-----------------"))
;     '((choice-item :value "--single-line"))
     (na-do-args (first data) (third data)) ;'member' branches and leaves
     '((choice-item :value "-----------------"))
;     '((choice-item :value "--single-line"))
     (na-do-args (second data) (fourth data))))) ;'other' branches and leaves


(define-widget 'node 'branch ; the inherited widget has to do with how
  ;to interpret the args when creating this widget
  "a meta-type representing either a leaf or a branch"
;  :mouse-down-action 'na-node-press-action
;  :action 'na-node-release-action
   :create 'na-node-create
;   :format "%[%t%]"
)

(defun na-node-create (widget)
  "Create a node by turning it into a leaf or a branch w/appropriate changes"
  (let ((path (widget-get widget :path))
	(orig widget)
; when working normal browsing, the following is not necessary, only when
; showing-all-angles!  Why?  How do I correct for it?
	(pool (or (widget-get widget :pool)
			 (widget-get (widget-get widget :parent) :pool)))
	(type (widget-type widget)))
    (cond ((or (eq type 'node) (eq type 'angle))
; the documentation is not true when it says that the :create function returns
; the widget.  it returns nil!  See how (widget-create) works.
	   (setq default-directory
		 (or path (concat default-directory (widget-get widget :tag))))
	   (when (eq type 'angle)
		 (widget-put widget :tag na-root-tag)) ; necessary?
	   ; need to add this widget to its own pool and set it's parent
	   ; pool accordingly;  Actually, need to add it to all the parent
	   ; pools.
	   (when pool ; probably the wrong stratgey for setting the pool
	     (widget-put widget :pool pool))
	   (widget-put widget :args (na-branch-arguments widget))
	   (setcdr orig (cdr (widget-convert widget)))
	   (setcar orig
		   (or
		    (or
		     (and (eq type 'angle) 'root)
		     (and (eq type 'node) 'branch))
		    type))))
    (funcall (na-default (widget-type widget) :create) orig)))


(define-widget 'angle 'branch
  "This is the meta-widget that create is called on.  It get's turned into a 'root
when it is initialized"
  :tag na-root-tag ; would like to change this into "angle # 1:" etc...
  :format "\n\n %[%t%]"
  :create 'na-node-create		;this may change
  :path na-base-directory
)

(define-widget 'root 'branch
  "What an 'angle gets turned into once it is initialized.  The corallary to 'branch
in the node/branch situation"
;  :create 'na-node-create
  :path na-base-directory
  :format "\n\n %[%t%]%v"
)


(define-widget 'either-or 'radio-button
  "Similar to radio buttons but both buttons cannot be 'off'"
  :action (lambda (widget)
	    (if (or (widget-get)))))

(defgroup na nil
  "N-Angulator editor and support library"
  :link '(custom-manual "(nd)Top")
  :link '(url-link :tag "Development Page"
		   "http://www.N-Angulator.org")
  :prefix "widget-"
  :group 'extensions
  :group 'hypermedia)


; untested

(defun na-anglepath (widget)
  "Return the string representing the path to this widget"
  (let* ((parent (widget-get widget :parent)))
    (if parent
	(mapconcat 'na-anglepath (list parent
				       (widget-value widget)) "/")
      na-base-directory)))


;;
;; Might make this use eshell someday
;;

(defun na-shell-read (command)
  "Execute shell 'command' in default-directory then 'read' in and
return the results"
  (read
   (setq after (exec-to-string
    (setq before command)))))

(defun na-shell-string-read (command)
  "Execute shell 'command' in default-directory then 'read' in and
return the results"
  (read (concat "\""
   (setq after (exec-to-string
    (setq before command))) "\"")))


(defun na-inodes (file)
  "Return a quote delimited multi-line string of the inodes under or representing
file/directory or if file, return its inode"
  (setq after (exec-to-string
   (setq before (format
    "find \"%s\" -type f -printf \"%%i\n\" 2>/dev/null|sort -u"
;    "(cd \"%s\" ;find \"%s\" -printf \"%%i\n\" 2>/dev/null|sort -u )" ; make it include directories
    file)))))

(defun na-isubtract (a b)
  "subtract the inodes in string b from the inodes in string b"
  (let* ((regexp (dired-string-replace-match	; build "or" regular expressions out of inodes
		    "..$" 
		    (concat "^" (dired-string-replace-match "\n" b "$|^" nil t))
		    "" nil t)))
    (na-shell-read ; strip out inodes
		   (format "echo -n \\\";echo -n \"%s\"|grep -v \"%s\"|echo \\\""
			   a regexp))))

(defun na-iadd (a b &optional c)
  "add sorted and uniquified the string(s) of inodes b to the string of inodes a"
  (na-shell-read
   (format "echo -n \\\";echo -n \"%s\"|sort|grep -v '^$'|uniq;echo -n \\\""
	   (apply 'concat a b c))))

(defun na-iinboth (a b)
  "return the string of inodes that is in both a and b"
  (na-shell-read
   (format "echo -n \\\";echo -n \"%s\"|sort|grep -v '^$'|uniq -d;echo -n \\\"" (concat a b))))


(defsubst na-allnames-unsplit (inode)
  "Return a list of strings of all the names
of file represented by inode, relative to the na-base-directory"
  (let ((default-directory na-base-directory))
     (na-shell-read
      (format
       "echo -n \\(; find . -inum %s -printf '\"%%p\" '; echo \\)"
       (number-to-string (read inode))))))

(defun na-allnames (inode)
  "Return a list of lists of strings representing all the components of names
of file represented by inode, relative to the na-base-directory"
  (let ((default-directory na-base-directory))
    (mapcar
     (lambda (x) (cdr (split-string x "/")))
     (na-allnames-unsplit inode))))


(defun na-all-angles-list (names path pool)
  "Generates the list compatible with widget-create given the result
from na-allnames and the pool of all inodes below na-base-directory.
If the pool is not nil, then the initial angle is created, otherwise
continuing nodes, leaves, and angles are created.  This function is
highly recursive and after entry, pool is not passed further down."
  (let* ((angle (car names))  ;the angle we are working on
	 (name (car angle))   ;name of next angle/leaf to construct
	 (bname (concat name "/")) ; the name as a branch
	 (bpath (concat path bname))) ; full path to the current node
    (if pool				;first time in
	`(angle :path ,na-base-directory  :value ,"/" ; was bname
		:pool ,pool
		:explicit-choice ,(na-all-angles-list names na-base-directory nil))
      (if (cdr angle)			;not leaf
	  `(node :tag ,bname :path ,bpath 
		 :explicit-choice ,(na-all-angles-list (cons (cdar names) (cdr names)) bpath nil))
	(append `(leaf :tag ,name)
		(if (cdr names)
		    `(:explicit-choice
		      (angle :path ,na-base-directory
			     :value ,(concat (caadr names) "/")
			     :explicit-choice
			     ,(na-all-angles-list
			       (cdr names) na-base-directory nil)))))))))
	       
; Get buffer to do widgets in

(defun na-getmake-buffer (buffname)
  "Get or make buffer to do widgets/na in"
  (kill-buffer (get-buffer-create buffname))
  (switch-to-buffer (get-buffer-create buffname))
  (kill-all-local-variables))

(defsubst na-error(text)
  "popup a dialog box with the error text if any"
  (unless (string= text "")
    (make-dialog-box 'question :modal nil :title "Error" 
		     :question text :buttons '(["Dismiss" '() t]))))

(defun na ()
  "Main entry to N-Angulator"
  (interactive)
  (message
   "Now running N-Angulator (c) 2011 TurbInfo.com -- All rights reserved")
  (na-getmake-buffer "*N-Angulator*") ; create a buffer with the files name

  (mailcap-parse-mailcaps nil t) ; force reparse

  (widget-create 'push-button
"                             Welcome to N-Angulator!                           ")
  (widget-insert "\n")
  (widget-create 'push-button
"          Persistent N-Dimensional Sparse Array, Editor, and Browser           ")
  (widget-insert "\n")
  (widget-create 'push-button
		 :action '(lambda (widget &optional event) (message "hit the copyright button"))
"            Copyright (c) 2011, TurbInfo.com - All Rights Reserved.            ")

  (widget-insert "\n\n                              ")
  (widget-create
   'push-button
   :action 'na-refresh-screen "Refresh Display")

  (widget-insert "\n\nCopy/Move/Link Selection:\n\n")

; uncomment these when not debugging
  (make-variable-buffer-local 'na-edit-mode)
  (make-variable-buffer-local 'na-base-directory)
  (make-variable-buffer-local 'na-control-directory)
  (make-variable-buffer-local 'na-root-tag)
  (make-variable-buffer-local 'na-new-angle-tag)
  (make-variable-buffer-local 'na-angles)
  (make-variable-buffer-local 'na-leaves)
  (make-variable-buffer-local 'na-need-br-rebuild)
  (make-variable-buffer-local 'na-selection)

  (setq na-selection (widget-create 'editable-field :size 67 :format "[%v]" ))

  (setq default-directory na-base-directory)

  (setq pool (na-inodes default-directory))

  (setq na-leaves '((dummy :pool "")))

  (setq na-angles (apply 'widget-create 'angle :path na-base-directory
		   :pool (na-inodes default-directory) 
	 '((item ""))))
  (widget-setup))

(defun na-lastnode (&optional widget)
  "Return the last node in the angles (be it root, branch or leaf)
Taking the :value of the returned widget will be the most reliable.  If
the :tag is nil but it has a value then it is a yet unclicked on branch"
  (let* ((angles (or widget na-angles))
	 (last-result angles)
	 (this-result angles))
    (while (setq this-result (car (widget-get this-result :children )))
      (setq last-result this-result))
    (widget-get last-result :parent)))

(provide 'na)
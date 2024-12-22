;;; dicom.el --- DICOM viewer - Digital Imaging & Communications in Medicine -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2024
;; Version: 0.4
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; URL: https://github.com/minad/dicom
;; Keywords: multimedia, hypermedia, files

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; DICOM stands for Digital Imaging and Communications in Medicine.  DICOM files
;; are typically used for medical imaging with different modalities like US, CR,
;; CT, MRI or PET.  This package adds the ability to view such files in Emacs.
;; The images and metadata are displayed in regular Emacs buffers.  The package
;; registers itself in `auto-mode-alist' and `magic-mode-alist' for DICOMDIR
;; directory files and DICOM images (file extension *.dcm or *.ima).
;; Furthermore the command `dicom-open' opens DICOMDIR directory files or DICOM
;; image files interactively.

;; Emacs must be compiled with support for PNG, SVG and XML.  The package relies
;; on external programs from the dcmtk DICOM toolkit, which are all widely
;; available on Linux distributions.

;; - `dcm2xml' and `dcmj2pnm' from the dcmtk DICOM toolkit
;; - `ffmpeg' for video conversion (optional)
;; - `mpv' for video playing (optional)

;;; Code:

(require 'compat)
(require 'dom)
(require 'outline)
(require 'image)
(require 'cus-edit)
(require 'subr-x)
(eval-when-compile (require 'cl-lib))

;;;; Customization

(defgroup dicom nil
  "DICOM viewer - Digital Imaging and Communications in Medicine."
  :link '(info-link :tag "Info Manual" "(dicom)")
  :link '(url-link :tag "Website" "https://github.com/minad/dicom")
  :link '(emacs-library-link :tag "Library Source" "dicom.el")
  :group 'files
  :group 'multimedia
  :prefix "dicom-")

(defcustom dicom-timeout 3
  "Timeout for conversion."
  :type 'natnum)

(defcustom dicom-parallel (num-processors)
  "Number of parallel conversion processes."
  :type 'natnum)

(defcustom dicom-attribute-width 25
  "Attribute name width."
  :type 'natnum)

(defcustom dicom-attribute-filter
  '( FileSetConsistencyFlag FileSetID
     IconImageSequence PrivateCreator
     RecordInUseFlag SpecificCharacterSet
     "\\`OffsetOf" "UID" " ")
  "Hidden DICOM attributes.
The list elements are either symbols or regular expressions."
  :type '(repeat (choice string symbol)))

(defcustom dicom-cache-dir (expand-file-name
                            (file-name-concat
                             (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                             "emacs/dicom/"))
  "Cache directory for converted images."
  :type 'string)

(defcustom dicom-play-command
  "(mpv --loop --osd-font-size=16 --osd-margin-x=0 --osd-margin-y=0 \
--osd-level=3 --osd-status-msg='fps:${container-fps} \
frame:${estimated-frame-number}/${estimated-frame-count} \
progress:${percent-pos}%%' %s) & disown"
  "Video player command line."
  :type 'string)

;;;; Faces

(defgroup dicom-faces nil
  "Faces used by DICOM."
  :group 'dicom
  :group 'faces)

(defface dicom-header
  '((t :inherit header-line :height 1.2 :weight bold))
  "Header line face.")

(defface dicom-title
  '((t :inherit header-line :extend t))
  "Item title face.")

;;;; Keymaps

(defvar-keymap dicom-image-map
  :doc "Keymap used for images at point."
  "RET" #'dicom-open-at-point
  "<mouse-1>" #'dicom-open-at-point)

(defvar-keymap dicom-mode-map
  :doc "Keymap used by `dicom-mode'."
  :parent special-mode-map
  "p" #'dicom-play
  "+" #'dicom-larger
  "-" #'dicom-smaller
  "r" #'dicom-rotate
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer)

(easy-menu-define dicom-mode-menu dicom-mode-map
  "Menu for `dicom-mode'."
  '("DICOM"
    ["Revert" revert-buffer]
    ["Larger" dicom-larger]
    ["Smaller" dicom-smaller]
    ["Rotate" dicom-rotate]
    ["Play" dicom-play]
    "--"
    ["Manual" (info "(dicom)")]
    ["Customize" (customize-group 'dicom)]))

(define-derived-mode dicom-mode special-mode "DICOM"
  "DICOM mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

;;;; Internal variables

(defvar-local dicom--data nil
  "Metadata of the current buffer.")

(defvar-local dicom--file nil
  "File associated with the current buffer.")

(defvar-local dicom--queue nil
  "Conversion process queue in current buffer.")

(defvar-local dicom--procs nil
  "Active conversion processes in current buffer.")

(defconst dicom--thumb
  '( :margin 8 :type svg :width 267 :height 200
     :data "<svg xmlns='http://www.w3.org/2000/svg' width='267' height='200'>
  <rect width='267' height='200' fill='black' stroke='gray'/>
  <line x1='0' y1='0' x2='267' y2='200' stroke='gray'/>
  <line x1='0' y1='200' x2='267' y2='0' stroke='gray'/>
</svg>")
  "Thumbnail placeholder image.")

;;;; Internal functions

(defun dicom--bookmark-record ()
  "Create DICOM bookmark."
  `(,(string-join (dicom--file-name))
    (filename . ,dicom--file)
    (handler . ,#'dicom-bookmark-jump)))

(defun dicom--stop (proc)
  "Gracefully stop PROC."
  (when proc
    (ignore-errors (signal-process proc 'TERM))
    (run-at-time 1 nil (lambda () (ignore-errors (delete-process proc))))))

(defun dicom--image-desc (file)
  "Image descriptor for FILE."
  `(image :margin 8 :type png :file ,file))

(defun dicom--dir-p (&optional file)
  "Non-nil if FILE is a DICOMDIR."
  (setq file (or file dicom--file))
  (and file (string-search "DICOMDIR" file)))

(defun dicom--file-name (&optional file)
  "Shortened FILE name."
  (setq file (or file dicom--file))
  (if (dicom--dir-p file)
      (list "dicom dir: "
            (file-name-base
             (directory-file-name
              (file-name-parent-directory file))))
    (list "dicom image: "
          (if-let ((dir (locate-dominating-file file "DICOMDIR")))
              (file-name-sans-extension
               (file-relative-name file (file-name-parent-directory dir)))
            (file-name-base file)))))

(defun dicom--buffer-name (file)
  "Buffer name for FILE."
  (format "*%s*" (string-join (dicom--file-name file))))

(defun dicom--cache-name (file &optional ext)
  "Cache file name given FILE name and EXT."
  (make-directory dicom-cache-dir t)
  (setq ext (or ext "png")
        file (file-name-concat dicom-cache-dir (md5 file)))
  (cons (concat file "." ext) (concat file ".tmp." ext)))

(defun dicom--convert-children (dom &optional tag)
  "Convert children of DOM with TAG."
  (delq nil (mapcar (lambda (x)
                      (and (or (not tag) (eq tag (dom-tag x)))
                           (dicom--convert x)))
                    (dom-children dom))))

(defun dicom--sort-alist (alist)
  "Sort ALIST by keys."
  (sort alist (lambda (x y) (string< (car x) (car y)))))

(defun dicom--convert (dom)
  "Convert DOM to nested lists."
  (pcase (dom-tag dom)
    ((or 'item 'data-set)
     (nconc (dicom--sort-alist (dicom--convert-children dom 'element))
            (dicom--sort-alist (dicom--convert-children dom 'sequence))))
    ('element
     (when-let ((name (dom-attr dom 'name))
                ((not (or (equal (dom-attr dom 'loaded) "no")
                          (equal (dom-attr dom 'binary) "hidden")
                          (let (case-fold-search)
                            (string-match-p dicom-attribute-filter name))))))
       (cons (intern name) (replace-regexp-in-string
                            "[ \t\n^]+" " " (dom-text dom)))))
    ('sequence
     (when-let ((name (dom-attr dom 'name))
                ((not (let (case-fold-search)
                        (string-match-p dicom-attribute-filter name))))
                (children (dicom--convert-children dom)))
       (cons (intern name) children)))))

(defun dicom--read (file)
  "Read DICOM FILE and return list of items."
  (with-temp-buffer
    (unless (eq 0 (call-process "dcm2xml" nil t nil
                                "--quiet" "--charset-assume"
                                "latin-1" "--convert-to-utf8" file))
      (error "DICOM: Reading DICOM metadata with dcm2xml failed"))
    (let ((dicom-attribute-filter (mapconcat (lambda (x) (format "%s" x))
                                             dicom-attribute-filter
                                             "\\|")))
      (dicom--convert (dom-child-by-tag (libxml-parse-xml-region) 'data-set)))))

(defun dicom--image-buffer ()
  "Return image buffer or throw an error."
  (if (dicom--dir-p)
      (or (get-buffer "*dicom image*")
          (user-error "DICOM: No open image"))
    (current-buffer)))

(defun dicom--modify-image (fun)
  "Modify image properties by FUN."
  (with-current-buffer (dicom--image-buffer)
    (when-let ((pos (text-property-not-all (point-min) (point-max) 'dicom--image nil))
               (image (get-text-property pos 'display)))
      (with-silent-modifications
        (goto-char pos)
        (funcall fun image)
        (put-text-property pos (1+ pos) 'display `(image ,@(cdr image)))))))

(defun dicom--run (cb &rest args)
  "Run process with ARGS asynchronously and call CB when the process finished."
  (let ((default-directory "/"))
    (push (make-process
           :name "dicom"
           :command args
           :noquery t
           :filter #'ignore
           :sentinel
           (let ((buf (current-buffer)))
             (lambda (proc event)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (cl-callf2 delq proc dicom--procs)
                   (funcall cb (string-prefix-p "finished" event))
                   (dicom--process))))))
          dicom--procs)
    (when dicom-timeout
      (run-at-time dicom-timeout nil #'dicom--stop (car dicom--procs)))))

(defun dicom--enqueue (&rest job)
  "Enqueue conversion JOB."
  (push job dicom--queue)
  (when (length< dicom--procs dicom-parallel)
    (dicom--process)))

(defun dicom--process ()
  "Process conversion queue."
  (setq mode-line-process (and dicom--queue
                               (format "[%d]" (length dicom--queue))))
  (when-let ((job (car (last dicom--queue))))
    (setq dicom--queue (nbutlast dicom--queue))
    (apply #'dicom--run job)))

(defun dicom--button (label action)
  "Insert button with LABEL and ACTION."
  (insert (propertize
           (format
            "  %s %s  "
            (key-description (where-is-internal action nil t t)) label)
           'keymap (define-keymap
                     "RET" action
                     "<down-mouse-1>" #'ignore
                     "<mouse-1>" (lambda (_event)
                                   (interactive "@e")
                                   (call-interactively action)))
           'face 'custom-button 'mouse-face 'custom-button-mouse)
          " "))

(defun dicom--title (level title)
  "Insert TITLE at LEVEL into buffer."
  (unless (or (bobp) (eq (char-before) ?\n))
    (insert "\n"))
  (insert
   (propertize
    (format "%s %s\n"
            (propertize (make-string level ?*) 'invisible t)
            title)
    'face (list 'dicom-title (intern (format "outline-%s" level))))))

(defun dicom--image-callback (tmp dst pos)
  "Job callback closure with TMP, DST and POS."
  (lambda (success)
    (if success
        (with-silent-modifications
          (rename-file tmp dst)
          (put-text-property pos (1+ pos) 'display (dicom--image-desc dst)))
      (delete-file tmp))))

(defun dicom--thumb (level item)
  "Insert ITEM with thumbnail at LEVEL into buffer."
  (pcase-let* ((src (expand-file-name
                     (string-replace "\\" "/" (alist-get 'ReferencedFileID item))))
               (`(,dst . ,tmp) (dicom--cache-name src))
               (exists (file-exists-p dst))
               (pos (point))
               (tooltip (progn
                          (dicom--item level item "")
                          (buffer-substring-no-properties pos (point)))))
    (delete-region pos (point))
    (insert (propertize
             " "
             'display (if exists (dicom--image-desc dst) `(image ,@dicom--thumb))
             'pointer 'hand
             'keymap dicom-image-map
             'dicom--file src
             'help-echo tooltip))
    (unless exists
      (dicom--enqueue
       (dicom--image-callback tmp dst pos)
       "dcmj2pnm" "--write-png" "--scale-y-size" "200" src tmp))))

(defun dicom--item (level item &optional indent)
  "Insert ITEM at LEVEL into buffer."
  (pcase-dolist (`(,k . ,v) item)
    (cond
     ((eq k 'DirectoryRecordSequence)
      (dolist (item v)
        (let ((type (alist-get 'DirectoryRecordType item)))
          (if (equal type "IMAGE")
              (dicom--thumb level item)
            (dicom--title level
                          (format "%s %s" type
                                  (or (alist-get 'StudyDescription item)
                                      (alist-get 'SeriesDescription item)
                                      (alist-get 'PatientName item)
                                      "")))
            (dicom--item level item)))))
     ((listp v)
      (let ((level (1+ level)))
        (dicom--title level k)
        (if (length= v 1)
            (dicom--item level (car v))
          (dolist (item v)
            (dicom--title (1+ level) "ITEM")
            (dicom--item (1+ level) item)))))
     ((not (eq k 'DirectoryRecordType))
      (let* ((k (symbol-name k))
             (s k))
        (when (> (length s) dicom-attribute-width)
          (setq s (truncate-string-to-width k dicom-attribute-width 0 nil "…"))
          (put-text-property 0 (length s) 'help-echo k s))
        (setq s (string-pad s dicom-attribute-width))
        (insert (or indent "    ") s "  " v "\n"))))))

(defun dicom--placeholder (w h)
  "Placeholder image with W and H."
  `(image
    :margin 8 :type svg :width ,w :height ,h
    :data
    ,(format
      "<svg xmlns='http://www.w3.org/2000/svg' width='%1$s' height='%2$s'>
  <rect width='%1$s' height='%2$s' fill='black' stroke='gray'/>
  <line x1='0' y1='0' x2='%1$s' y2='%2$s' stroke='gray'/>
  <line x1='0' y1='%2$s' x2='%1$s' y2='0' stroke='gray'/>
</svg>" w h)))

(defun dicom--image ()
  "Insert large image."
  (insert (propertize "\n" 'face '(:height 0.2)))
  (dicom--button "Revert" #'revert-buffer)
  (dicom--button "Larger" #'dicom-larger)
  (dicom--button "Smaller" #'dicom-smaller)
  (dicom--button "Rotate" #'dicom-rotate)
  (when-let ((frames (alist-get 'NumberOfFrames dicom--data)))
    (dicom--button (format "Play (%s frames)" frames) #'dicom-play))
  (insert "\n" (propertize "\n" 'face '(:height 0.2)))
  (pcase-let* ((`(,dst . ,tmp) (dicom--cache-name (concat "large" dicom--file)))
               (exists (file-exists-p dst))
               (pos (point)))
    (insert (propertize
             " "
             'dicom--image t
             'pointer 'arrow
             'display (if exists
                          (dicom--image-desc dst)
                        (dicom--placeholder
                         (alist-get 'Columns dicom--data 800)
                         (alist-get 'Rows dicom--data 600))))
            "\n")
    (unless exists
      (dicom--enqueue
       (dicom--image-callback tmp dst pos)
       "dcmj2pnm" "--write-png" dicom--file tmp))))

(defun dicom--setup-check ()
  "Check requirements."
  (let (req)
    (unless (display-graphic-p)
      (push "graphical display" req))
    (unless (libxml-available-p)
      (push "libxml" req))
    (dolist (type '(png svg))
      (unless (image-type-available-p type)
        (push (format "lib%s" type) req)))
    (dolist (exe '("dcm2xml" "dcmj2pnm"))
      (unless (executable-find exe)
        (push exe req)))
    (when req
      (error "DICOM: %s required to proceed" (string-join req ", ")))))

(defun dicom--setup-locals (file)
  "Initialize buffer locals for FILE."
  (setq-local dicom--queue nil
              dicom--procs nil
              dicom--file file
              dicom--data (dicom--read file)
              buffer-read-only t
              truncate-lines nil
              bookmark-make-record-function #'dicom--bookmark-record
              revert-buffer-function (lambda (&rest _) (dicom--setup file))
              fringe-indicator-alist '((continuation . nil)
                                       (truncation . nil))
              outline-regexp "\\*+"
              outline-minor-mode-cycle t
              outline-minor-mode-use-buttons 'in-margins
              header-line-format
              (format (propertize " DICOM %s %s" 'face 'dicom-header)
                      (if (dicom--dir-p) "DIR" "IMAGE")
                      (cadr (dicom--file-name)))))

(defun dicom--setup-content ()
  "Setup buffer content."
  (with-silent-modifications
    (erase-buffer)
    (unless (dicom--dir-p)
      (dicom--image)
      (dicom--title 1 "IMAGE"))
    (dicom--item 1 dicom--data)
    (goto-char (point-min))))

(defun dicom--setup (file)
  "Setup buffer for FILE."
  (condition-case err
      (progn
        (dicom--setup-check)
        (mapc #'dicom--stop dicom--procs)
        (dicom-mode)
        (dicom--setup-locals file)
        (dicom--setup-content)
        (outline-minor-mode))
    (error
     (kill-buffer)
     (signal (car err) (cdr err)))))

;;;; Public commands

(defun dicom-rotate ()
  "Rotate image by 90°."
  (interactive nil dicom-mode)
  (dicom--modify-image
   (lambda (image)
     (setf (image-property image :rotation)
           (float (mod (+ (or (image-property image :rotation) 0) 90) 360))))))

(defun dicom-larger (n)
  "Image larger by N."
  (interactive "p" dicom-mode)
  (dicom--modify-image
   (lambda (image)
     (setf (image-property image :scale)
           (max 0.1 (min 10 (+ (* n 0.1) (or (image-property image :scale) 1.0))))))))

(defun dicom-smaller (n)
  "Image smaller by N."
  (interactive "p" dicom-mode)
  (dicom-larger (- n)))

(defun dicom-play ()
  "Play DICOM multi frame image."
  (interactive nil dicom-mode)
  (with-current-buffer (dicom--image-buffer)
    (pcase-let ((`(,dst . ,tmp) (dicom--cache-name dicom--file "mp4")))
      (cond
       ((file-exists-p dst)
        (message "Playing %s…" (abbreviate-file-name dicom--file))
        (call-process-shell-command
         (format dicom-play-command (shell-quote-argument dst))
         nil 0))
       (dicom--procs
        (message "Conversion in progress…"))
       (t
        (unless (alist-get 'NumberOfFrames dicom--data)
          (user-error "DICOM: No multi frame image"))
        (message "Converting %s…" (abbreviate-file-name dicom--file))
        (let (dicom-timeout)
          (dicom--enqueue
           (lambda (success)
             (if success
                 (progn
                   (rename-file tmp dst)
                   (dicom-play))
               (delete-file tmp)))
           "sh" "-c"
           (format
            "dcmj2pnm --all-frames --write-bmp %s | ffmpeg -framerate %s -i - %s"
            (shell-quote-argument dicom--file)
            (or (alist-get 'RecommendedDisplayFrameRate dicom--data)
                (alist-get 'CineRate dicom--data)
                25)
            (shell-quote-argument tmp)))))))))

;;;###autoload
(defun dicom-open-at-point ()
  "Open DICOM at point."
  (interactive)
  (if-let ((file
            (if (mouse-event-p last-input-event)
                (or (mouse-posn-property (event-start last-input-event)
                                         'dicom--file)
                    (thing-at-mouse last-input-event 'filename))
              (or (get-text-property (point) 'dicom--file)
                  (thing-at-point 'filename)))))
      (dicom-open file (and (not last-prefix-arg) "*dicom image*"))
    (user-error "DICOM: No DICOM file at point")))

;;;###autoload
(defun dicom-open (file &optional reuse)
  "Open DICOM dir or image FILE.
REUSE can be a buffer name to reuse."
  (interactive "fDICOM: ")
  (let* ((file (expand-file-name (if (directory-name-p file)
                                     (file-name-concat file "DICOMDIR")
                                   file)))
         (default-directory (file-name-directory file))
         (buf (or reuse (dicom--buffer-name file))))
    (unless (file-regular-p file)
      (user-error "DICOM: File %s not found" file))
    (unless (when-let ((buf (get-buffer buf)))
              (equal (buffer-local-value 'dicom--file buf) file))
      (with-current-buffer (get-buffer-create buf)
        (dicom--setup file)))
    (if reuse
        (display-buffer buf '(nil (inhibit-same-window . t)))
      (pop-to-buffer buf))))

;;;###autoload
(defun dicom-bookmark-jump (bm)
  "Jump to DICOM bookmark BM."
  (declare-function bookmark-get-filename "bookmark")
  (dicom-open (bookmark-get-filename bm)))
(put 'dicom-bookmark-jump 'bookmark-handler-type "DICOM")

;;;###autoload
(defun dicom-auto-mode ()
  "Enable `dicom-mode' in current buffer."
  (let ((file (expand-file-name buffer-file-name)))
    (setq-local buffer-file-name nil
                buffer-file-truename nil)
    (rename-buffer (dicom--buffer-name file) t)
    (dicom--setup file)))

;;;###autoload
(progn
  (defun dicom--magic-p ()
    (save-excursion (goto-char 129) (looking-at-p "DICM")))
  (add-to-list 'magic-mode-alist '(dicom--magic-p . dicom-auto-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:dcm\\|ima\\)\\'" . dicom-auto-mode))
  (add-to-list 'auto-mode-alist '("DICOMDIR" . dicom-auto-mode)))

;;;###autoload
(funcall 'eval-after-load 'ol
  (lambda ()
    (defvar dicom--file)
    (declare-function org-link-set-parameters "ol")
    (declare-function org-link-store-props "ol")
    (org-link-set-parameters
     "dicom"
     :follow (lambda (link _) (dicom-open link))
     :store
     (lambda ()
       (when (eq major-mode 'dicom-mode)
         (org-link-store-props
          :type "dicom"
          :link (concat "dicom:" (abbreviate-file-name dicom--file))))))))

(provide 'dicom)
;;; dicom.el ends here

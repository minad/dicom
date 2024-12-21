;;; dicom.el --- DICOM viewer - Digital Imaging & Communications in Medicine -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2024
;; Version: 0.2
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

;; Emacs must be compiled with support for PNG, XML and SVG.  The package relies
;; on a few external programs, which are all widely available on Linux
;; distributions.

;; - `convert' from the ImageMagick suite
;; - `dcm2xml' and `dcmj2pnm' from the dcmtk DICOM toolkit
;; - `ffmpeg' for video conversion (optional)
;; - `mpv' for video playing (optional)

;;; Code:

;; TODO Lossless JPEG DICOM cannot be converted by ImageMagick

(require 'compat)
(require 'dom)
(require 'outline)
(require 'image)
(require 'cus-edit)
(require 'subr-x)

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

(defcustom dicom-field-width 25
  "Field width."
  :type 'natnum)

(defcustom dicom-hidden-fields
  '( SpecificCharacterSet
     DirectoryRecordType
     OffsetOfReferencedLowerLevelDirectoryEntity
     OffsetOfTheNextDirectoryRecord
     RecordInUseFlag
     PrivateCreator)
  "List of hidden DICOM properties."
  :type '(repeat symbol))

(defcustom dicom-cache-dir (expand-file-name
                            (file-name-concat
                             (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                             "emacs/dicom/"))
  "Cache directory for converted images."
  :type 'string)

(defcustom dicom-play-command
  "(mpv --loop --osd-font-size=16 --osd-margin-x=0 --osd-margin-y=0 --osd-level=3 \
--osd-status-msg='fps:${container-fps} \
frame:${estimated-frame-number}/${estimated-frame-count} \
progress:${percent-pos}%%' %s) & disown"
  "Video player command line."
  :type 'string)

(defgroup dicom-faces nil
  "Faces used by DICOM."
  :group 'dicom
  :group 'faces)

(defface dicom-header
  '((t :inherit header-line :height 1.2 :weight bold))
  "Header line face.")

(defface dicom-item
  '((t :inherit (header-line outline-2) :extend t))
  "Item face.")

(defvar-local dicom--data nil
  "DICOM data of the current buffer.")

(defvar-local dicom--file nil
  "DICOM file associated with the current buffer.")

(defvar-local dicom--queue nil
  "Conversion process queue in current buffer.")

(defvar-local dicom--proc nil
  "Active conversion process in current buffer.")

(defconst dicom--thumb-placeholder
  '( :margin 8 :type svg :width 267 :height 200
     :data "<svg xmlns='http://www.w3.org/2000/svg' width='267' height='200'>
  <rect width='267' height='200' fill='black' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='0' x2='267' y2='200' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='200' x2='267' y2='0' stroke='gray' stroke-width='1'/>
</svg>")
  "Thumbnail placeholder image.")

(defconst dicom--large-placeholder
  (propertize
   " "
   'dicom--image t
   'pointer 'arrow
   'display
   '(image :margin 8 :type svg :width 800 :height 600
           :data "<svg xmlns='http://www.w3.org/2000/svg' width='800' height='600'>
  <rect width='800' height='600' fill='black' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='0' x2='800' y2='600' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='600' x2='800' y2='0' stroke='gray' stroke-width='1'/>
</svg>"))
  "Large placeholder image.")

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

(defun dicom--stop (proc)
  "Gracefully stop PROC."
  (when proc
    (ignore-errors (signal-process proc 'TERM))
    (run-at-time 1 nil (lambda () (ignore-errors (delete-process proc))))))

(defun dicom--put-image (pos file)
  "Display image FILE at POS."
  (with-silent-modifications
    (put-text-property pos (1+ pos) 'display
                       `(image :margin 8 :type png :file ,file))))

(defun dicom--cache-name (file &optional ext)
  "Cache file name given FILE name and EXT."
  (make-directory dicom-cache-dir t)
  (setq ext (or ext "png")
        file (file-name-concat dicom-cache-dir (md5 file)))
  (cons (concat file "." ext) (concat file ".tmp." ext)))

(defun dicom--insert (item)
  "Insert ITEM in buffer."
  (let ((type (alist-get 'DirectoryRecordType item)))
    (insert "\n" (format
                  (propertize " %s %s\n" 'face 'dicom-item)
                  (or type "Item")
                  (or (and type (or (alist-get 'StudyID item)
                                    (alist-get 'SeriesDescription item)
                                    (alist-get 'PatientName item)))
                      ""))))
  (pcase-dolist (`(,k . ,v) item)
    (unless (memq k dicom-hidden-fields)
      (let* ((k (symbol-name k))
             (s k))
        (when (> (length s) dicom-field-width)
          (setq s (truncate-string-to-width k dicom-field-width 0 nil "…"))
          (put-text-property 0 (length s) 'help-echo k s))
        (setq s (string-pad s dicom-field-width))
        (insert (format "    %s  %s\n" s v))))))

(defun dicom--insert-all ()
  "Insert all items into buffer."
  (dolist (item dicom--data)
    (let ((pos (point)))
      (dicom--insert item)
      (when (equal (alist-get 'DirectoryRecordType item) "IMAGE")
        (pcase-let* ((src (expand-file-name
                           (string-replace "\\" "/" (alist-get 'ReferencedFileID item))))
                     (`(,dst . ,tmp) (dicom--cache-name src))
                     (tooltip (buffer-substring-no-properties (1+ pos) (point))))
          (delete-region pos (point))
          (insert (propertize
                   " " 'display `(image ,@dicom--thumb-placeholder)
                   'pointer 'hand
                   'keymap dicom-image-map
                   'dicom--file src
                   'help-echo tooltip))
          (if (file-exists-p dst)
              (dicom--put-image pos dst)
            (dicom--enqueue
             (lambda (success)
               (if success
                   (progn
                     (rename-file tmp dst)
                     (dicom--put-image pos dst))
                 (delete-file tmp)))
             "dcmj2pnm" "--write-png" "--scale-y-size" "200" src tmp)))))))

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

(defun dicom--insert-large ()
  "Insert large image."
  (pcase-let ((`(,dst . ,tmp) (dicom--cache-name (concat "large" dicom--file))))
    (insert "\n")
    (dicom--button "Revert" #'revert-buffer)
    (dicom--button "Larger" #'dicom-larger)
    (dicom--button "Smaller" #'dicom-smaller)
    (dicom--button "Rotate" #'dicom-rotate)
    (when-let ((frames (alist-get 'NumberOfFrames (car dicom--data))))
      (dicom--button (format "Play (%s frames)" frames) #'dicom-play))
    (insert "\n\n")
    (let ((pos (point)))
      (insert dicom--large-placeholder "\n")
      (if (file-exists-p dst)
        (dicom--put-image pos dst)
      (dicom--enqueue
       (lambda (success)
         (if success
             (progn
               (rename-file tmp dst)
               (dicom--put-image pos dst))
           (delete-file tmp)))
       "dcmj2pnm" "--write-png" dicom--file tmp)))))

(defun dicom--read (file)
  "Read DICOM FILE and return list of items."
  (let ((dom (with-temp-buffer
               (unless (eq 0 (call-process "dcm2xml" nil t nil
                                           "--quiet" "--charset-assume"
                                           "latin-1" "--convert-to-utf8" file))
                 (error "DICOM: Reading DICOM metadata with dcm2xml failed"))
               (libxml-parse-xml-region)))
        (items nil))
    (dolist (item (append (and (not (dicom--dir-p file))
                               (dom-by-tag dom 'data-set))
                          (dom-by-tag dom 'item)))
      (let (alist (hidden t))
        (dolist (elem (dom-children item))
          (let ((name (dom-attr elem 'name)))
            (unless (or (not (eq (dom-tag elem) 'element))
                        (equal (dom-attr elem 'loaded) "no")
                        (equal (dom-attr elem 'binary) "hidden")
                        (string-search "UID" name)
                        (string-search " " name))
              (setq name (intern name))
              (unless (memq name dicom-hidden-fields)
                (setq hidden nil))
              (push (cons name (replace-regexp-in-string
                                "\\s-+" " "
                                (string-replace "^" " " (dom-text elem))))
                    alist))))
        (unless hidden
          (push (sort alist (lambda (x y) (string< (car x) (car y)))) items))))
    (nreverse items)))

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

(defun dicom--image-buffer ()
  "Return image buffer or throw an error."
  (if (dicom--dir-p)
      (or (get-buffer "*dicom image*")
          (user-error "DICOM: No open image"))
    (current-buffer)))

(defun dicom-rotate ()
  "Rotate image by 90°."
  (interactive nil dicom-mode)
  (dicom--modify-image
   (lambda (image)
     (setf (image-property image :rotation)
           (float (mod (+ (or (image-property image :rotation) 0) 90) 360))))))

(defun dicom--modify-image (fun)
  "Modify image properties by FUN."
  (with-current-buffer (dicom--image-buffer)
   (when-let ((pos (text-property-not-all (point-min) (point-max) 'dicom--image nil))
              (image (get-text-property pos 'display)))
     (with-silent-modifications
       (funcall fun image)
       (put-text-property pos (1+ pos) 'display `(image ,@(cdr image)))))))

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

(defun dicom--run (cb &rest args)
  "Run process with ARGS asynchronously and call CB when the process finished."
  (let ((default-directory "/"))
    (setq dicom--proc
          (make-process
           :name "dicom"
           :command args
           :noquery t
           :filter #'ignore
           :sentinel
           (let ((buf (current-buffer)))
             (lambda (_proc event)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq dicom--proc nil)
                   (funcall cb (string-prefix-p "finished" event))
                   (dicom--process)))))))
    (when dicom-timeout
      (run-at-time dicom-timeout nil #'dicom--stop dicom--proc))))

(defun dicom--enqueue (&rest job)
  "Enqueue conversion JOB."
  (push job dicom--queue)
  (unless dicom--proc (dicom--process)))

(defun dicom--process ()
  "Process conversion queue."
  (setq mode-line-process (and dicom--queue
                               (format "[%d]" (length dicom--queue))))
  (when-let ((job (car (last dicom--queue))))
    (setq dicom--queue (nbutlast dicom--queue))
    (apply #'dicom--run job)))

(defun dicom-play ()
  "Play DICOM multi frame image."
  (interactive nil dicom-mode)
  (with-current-buffer (dicom--image-buffer)
   (pcase-let ((`(,dst . ,tmp) (dicom--cache-name dicom--file "mp4")))
     (cond
      ((file-exists-p dst)
       (message "Playing %s…" dicom--file)
       (call-process-shell-command
        (format dicom-play-command (shell-quote-argument dst))
        nil 0))
      (dicom--proc
       (message "Conversion in progress…"))
      (t
       (unless (alist-get 'NumberOfFrames (car dicom--data))
         (user-error "DICOM: No multi frame image"))
       (let ((rate (or (alist-get 'RecommendedDisplayFrameRate (car dicom--data))
                       (alist-get 'CineRate (car dicom--data))
                       25))
             dicom-timeout)
         (message "Converting %s…" dicom--file)
         (dicom--enqueue
          (lambda (success)
            (if success
                (progn
                  (rename-file tmp dst)
                  (dicom-play))
              (delete-file tmp)))
          "sh" "-c"
          (format "convert %s ppm:- | ffmpeg -framerate %s -i - %s"
                  (shell-quote-argument dicom--file)
                  rate
                  (shell-quote-argument tmp)))))))))

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
    (dolist (exe '("dcm2xml" "dcmj2pnm" "convert"))
      (unless (executable-find exe)
        (push exe req)))
    (when req
      (error "DICOM: %s required to proceed" (string-join req ", ")))))

(defun dicom--setup-locals (file)
  "Initialize buffer locals for FILE."
  (setq-local dicom--queue nil
              dicom--proc nil
              dicom--file file
              dicom--data (dicom--read file)
              buffer-read-only t
              truncate-lines nil
              bookmark-make-record-function #'dicom--bookmark-record
              revert-buffer-function (lambda (&rest _) (dicom--setup file))
              fringe-indicator-alist '((continuation . nil)
                                       (truncation . nil))
              outline-regexp " [A-Z]"
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
      (dicom--insert-large))
    (dicom--insert-all)
    (goto-char (point-min))))

(defun dicom--setup (file)
  "Setup buffer for FILE."
  (condition-case err
      (progn
        (dicom--setup-check)
        (dicom--stop dicom--proc)
        (dicom-mode)
        (dicom--setup-locals file)
        (dicom--setup-content)
        (outline-minor-mode))
    (error
     (kill-buffer)
     (signal (car err) (cdr err)))))

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

;;;###autoload
(defun dicom-auto-mode ()
  "Enable `dicom-mode' in current buffer."
  (let ((file (expand-file-name buffer-file-name)))
    (setq-local buffer-file-name nil
                buffer-file-truename nil)
    (rename-buffer (dicom--buffer-name file) t)
    (dicom--setup file)))

;;;###autoload
(defun dicom-bookmark-jump (bm)
  "Jump to DICOM bookmark BM."
  (declare-function bookmark-get-filename "bookmark")
  (dicom-open (bookmark-get-filename bm)))
(put 'dicom-bookmark-jump 'bookmark-handler-type "DICOM")

(defun dicom--bookmark-record ()
  "Create DICOM bookmark."
  `(,(string-join (dicom--file-name))
    (filename . ,dicom--file)
    (handler . ,#'dicom-bookmark-jump)))

;;;###autoload
(progn
  (defun dicom--magic-p ()
    (and (> (point-max) 133) (equal "DICM" (buffer-substring 129 133))))
  (add-to-list 'magic-mode-alist '(dicom--magic-p . dicom-auto-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:dcm\\|ima\\)\\'" . dicom-auto-mode))
  (add-to-list 'auto-mode-alist '("DICOMDIR" . dicom-auto-mode)))

(provide 'dicom)
;;; dicom.el ends here

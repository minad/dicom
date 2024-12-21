;;; dicom.el --- DICOM viewer -*- lexical-binding: t -*-

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

;; DICOM files are typically used for medical imaging (US, CT, MRI, PET).  This
;; package adds the ability to view such files in Emacs.  The images and
;; metadata are displayed in regular Emacs buffers.  The package registers
;; itself in `auto-mode-alist' and `magic-mode-alist' for DICOMDIR directory
;; files and DICOM images (file extension *.dcm or *.ima).  Furthermore the
;; command `dicom-open' opens DICOMDIR directory files or DICOM image files
;; interactively.

;; Emacs must be compiled with support for PNG, XML and SVG.  The package relies
;; on a few external programs, which are all widely available on Linux
;; distributions.

;; - `convert' from the ImageMagick suite
;; - `dcm2xml' from the dcmtk DICOM toolkit
;; - `ffmpeg' for video conversion (optional)
;; - `mpv' for video playing (optional)

;;; Code:

;; TODO Lossless JPEG DICOM cannot be converted by ImageMagick

(require 'compat)
(require 'dom)
(require 'outline)
(eval-when-compile (require 'subr-x))

(defvar dicom--hidden '( SpecificCharacterSet
                         DirectoryRecordType
                         OffsetOfReferencedLowerLevelDirectoryEntity
                         OffsetOfTheNextDirectoryRecord
                         RecordInUseFlag
                         PrivateCreator)
  "List of hidden DICOM properties.")

(defvar dicom--cache-dir (expand-file-name
                          (file-name-concat
                           (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                           "emacs/dicom/"))
  "Cache directory for converted images.")

(defvar dicom--mpv-args
  "--loop --osd-font-size=16 --osd-margin-x=0 --osd-margin-y=0 --osd-level=3 \
--osd-status-msg='fps:${container-fps} \
frame:${estimated-frame-number}/${estimated-frame-count} \
progress:${percent-pos}%'"
  "MPV command line arguments.")

(defvar dicom--timeout 3
  "Timeout for conversion.")

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
   'pointer 'arrow
   'display
   '(image :margin 8 :type svg :width 800 :height 600
           :data "<svg xmlns='http://www.w3.org/2000/svg' width='800' height='600'>
  <rect width='800' height='600' fill='black' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='0' x2='800' y2='600' stroke='gray' stroke-width='1'/>
  <line x1='0' y1='600' x2='800' y2='0' stroke='gray' stroke-width='1'/>
</svg>"))
  "Large placeholder image.")

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

(defun dicom--async (cb &rest args)
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
                   (funcall cb (string-prefix-p "finished" event))))))))
    (when dicom--timeout
      (run-at-time dicom--timeout nil #'dicom--stop dicom--proc))))

(defun dicom--cache-name (file &optional ext)
  "Cache file name given FILE name and EXT."
  (make-directory dicom--cache-dir t)
  (setq ext (or ext "png")
        file (file-name-concat dicom--cache-dir (md5 file)))
  (cons (concat file "." ext) (concat file ".tmp." ext)))

(defun dicom--insert (item)
  "Insert ITEM in buffer."
  (let ((type (alist-get 'DirectoryRecordType item)))
    (insert "\n" (format
                  (propertize " %s %s\n" 'face '(:inherit (header-line outline-2) :extend t))
                  (or type "Item")
                  (or (and type (or (alist-get 'StudyID item)
                                    (alist-get 'SeriesDescription item)
                                    (alist-get 'PatientName item)))
                      ""))))
  (pcase-dolist (`(,k . ,v) item)
    (unless (memq k dicom--hidden)
      (let ((k (symbol-name k)))
        (when (> (length k) 25)
          (setq k (propertize
                   (truncate-string-to-width k 25 0 nil "…")
                   'help-echo k)))
        (insert (format "    %-25s  %s\n" k v))))))

(defun dicom--read (file)
  "Read DICOM FILE and return list of items."
  (let ((dom (with-temp-buffer
               (unless (eq 0 (call-process "dcm2xml" nil t nil
                                           "--quiet" "--charset-assume"
                                           "latin-1" "--convert-to-utf8" file))
                 (error "DICOM: Reading DICOM metadata with dcm2xml failed"))
               (libxml-parse-xml-region)))
        (items nil))
    (dolist (item (append (and (not (string-suffix-p "DICOMDIR" file))
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
              (unless (memq name dicom--hidden)
                (setq hidden nil))
              (push (cons name (string-replace "^" " " (dom-text elem))) alist))))
        (unless hidden
          (push (sort alist (lambda (x y) (string< (car x) (car y)))) items))))
    (nreverse items)))

(defun dicom-open-at-point ()
  "Open image at point."
  (declare (completion ignore))
  (interactive)
  (if-let ((file
            (if (mouse-event-p last-input-event)
                (mouse-posn-property (event-start last-input-event)
                                     'dicom--file)
              (get-text-property (point) 'dicom--file))))
      (dicom-open file (and (not last-prefix-arg) "*dicom image*"))
    (user-error "DICOM: No image at point")))

(defvar-keymap dicom-mode-map
  :doc "Keymap used by `dicom-dir-mode' and `dicom-image-mod'."
  :parent special-mode-map
  "p" #'dicom-play
  "+" #'dicom-larger
  "-" #'dicom-smaller
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer)

(defvar-keymap dicom-dir-mode-map
  :doc "Keymap used by `dicom-dir-mode'."
  :parent dicom-mode-map
  "RET" #'dicom-open-at-point
  "<mouse-1>" #'dicom-open-at-point)

(defvar-keymap dicom-image-mode-map
  :doc "Keymap used by `dicom-image-mode'."
  :parent dicom-mode-map)

(easy-menu-define dicom-image-mode-menu dicom-image-mode-map
  "Menu for `dicom-image-mode'."
  '("DICOM IMAGE"
    ["Larger" dicom-larger]
    ["Smaller" dicom-smaller]
    ["Play" dicom-play]))

(defmacro dicom--image-buffer (&rest body)
  "Run BODY inside image buffer if it exists."
  `(with-current-buffer (if (eq major-mode #'dicom-image-mode)
                            (current-buffer)
                          (or (get-buffer "*dicom image*")
                              (user-error "DICOM: No open image")))
     ,@body))

(defun dicom-larger (n)
  "Image larger by N."
  (interactive "p" dicom-dir-mode dicom-image-mode)
  (dicom--image-buffer
   (when-let ((pos (text-property-not-all (point-min) (point-max) 'display nil))
              (image (cdr (get-text-property pos 'display))))
     (setf (plist-get image :scale)
           (max 0.1 (min 10 (+ (* n 0.1) (or (plist-get image :scale) 1.0)))))
     (with-silent-modifications
       (put-text-property pos (1+ pos) 'display `(image ,@image))))))

(defun dicom-smaller (n)
  "Image smaller by N."
  (interactive "p" dicom-dir-mode dicom-image-mode)
  (dicom-larger (- n)))

(define-derived-mode dicom-mode special-mode "DICOM"
  "DICOM mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

(define-derived-mode dicom-dir-mode dicom-mode "DICOM DIR"
  "DICOM DIR mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

(define-derived-mode dicom-image-mode dicom-mode "DICOM IMAGE"
  "DICOM IMAGE mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

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
        (display-buffer buf)
      (pop-to-buffer buf))))

(defun dicom--process-queue ()
  "Process conversion queue."
  (setq mode-line-process (and dicom--queue
                               (format "[%d]" (length dicom--queue))))
  (pcase (pop dicom--queue)
    (`(,pos ,dst ,tmp ,src)
     (dicom--async (lambda (success)
                     (if success
                         (progn
                           (rename-file tmp dst)
                           (dicom--put-image pos dst))
                       (delete-file tmp))
                     (dicom--process-queue))
                   "convert" src "-delete" "1--1" "-thumbnail" "x200" tmp))))

(defun dicom-play ()
  "Play DICOM multi frame image."
  (interactive nil dicom-dir-mode dicom-image-mode)
  (dicom--image-buffer
   (pcase-let ((`(,dst . ,tmp) (dicom--cache-name dicom--file "mp4")))
     (cond
      ((file-exists-p dst)
       (message "Playing %s…" dicom--file)
       (call-process-shell-command
        (format "(mpv %s %s) & disown" dicom--mpv-args (shell-quote-argument dst))
        nil 0))
      (dicom--proc
       (message "Conversion in progress…"))
      (t
       (unless (alist-get 'NumberOfFrames (car dicom--data))
         (user-error "DICOM: No multi frame image"))
       (let ((rate (or (alist-get 'RecommendedDisplayFrameRate (car dicom--data))
                       (alist-get 'CineRate (car dicom--data))
                       25))
             dicom--timeout)
         (message "Converting %s…" dicom--file)
         (dicom--async (lambda (success)
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

(defun dicom--setup (file)
  "Setup buffer for FILE."
  (let (req)
    (unless (display-graphic-p)
      (push "graphical display" req))
    (unless (image-type-available-p 'png)
      (push "libpng" req))
    (unless (image-type-available-p 'svg)
      (push "libsvg" req))
    (unless (libxml-available-p)
      (push "libxml" req))
    (unless (executable-find "dcm2xml")
      (push "dcm2xml" req))
    (unless (executable-find "convert")
      (push "convert" req))
    (when req
      (error "DICOM: %s required to proceed" (string-join req ", "))))
  (dicom--stop dicom--proc)
  (if (string-suffix-p "DICOMDIR" file) (dicom-dir-mode) (dicom-image-mode))
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
              (format (propertize
                       " %s %s"
                       'face '(:inherit header-line :height 1.5 :weight bold))
                      mode-name (cadr (dicom--file-name))))
  (with-silent-modifications
    (erase-buffer)
    (funcall (intern (format "%s--setup" major-mode)))
    (goto-char (point-min))
    (outline-minor-mode)))

(defun dicom-image-mode--setup ()
  "Setup `dicom-image-mode' buffer."
  (pcase-let* ((`(,dst . ,tmp) (dicom--cache-name (concat "large" dicom--file)))
               (pos nil))
    (insert "\n")
    (insert-button "+ LARGER" 'action (lambda (_) (dicom-larger 1)))
    (insert " | ")
    (insert-button "- SMALLER" 'action (lambda (_) (dicom-smaller 1)))
    (when-let ((frames (alist-get 'NumberOfFrames (car dicom--data))))
      (insert " | ")
      (insert-button (format "p PLAY %s FRAMES" frames)
                     'action (lambda (_) (dicom-play))))
    (insert "\n")
    (setq pos (point))
    (insert dicom--large-placeholder "\n")
    (mapc #'dicom--insert dicom--data)
    (if (file-exists-p dst)
        (dicom--put-image pos dst)
      (dicom--async (lambda (success)
                      (if success
                          (progn
                            (rename-file tmp dst)
                            (dicom--put-image pos dst))
                        (delete-file tmp)))
                    "convert" dicom--file "-delete" "1--1" tmp))))

(defun dicom-dir-mode--setup ()
  "Setup `dicom-dir-mode' buffer."
  (dolist (item dicom--data)
    (let ((type (alist-get 'DirectoryRecordType item))
          (pos (point)))
      (dicom--insert item)
      (when (equal type "IMAGE")
        (pcase-let* ((src (expand-file-name
                           (string-replace "\\" "/" (alist-get 'ReferencedFileID item))))
                     (`(,dst . ,tmp) (dicom--cache-name src))
                     (tooltip (buffer-substring-no-properties (1+ pos) (point))))
          (delete-region pos (point))
          (insert (propertize
                   " " 'display `(image ,@dicom--thumb-placeholder)
                   'pointer 'arrow
                   'dicom--file src
                   'help-echo tooltip))
          (if (file-exists-p dst)
              (dicom--put-image pos dst)
            (push (list pos dst tmp src) dicom--queue))))))
  (setq dicom--queue (nreverse dicom--queue))
  (dicom--process-queue))

(defun dicom--file-name (&optional file)
  "Shortened FILE name."
  (setq file (or file dicom--file))
  (if (string-suffix-p "DICOMDIR" file)
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
  "Enable either `dicom-image-mode' or `dicom-dir-mode' in current buffer."
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
  (add-to-list 'auto-mode-alist '("DICOMDIR\\'" . dicom-auto-mode)))

(provide 'dicom)
;;; dicom.el ends here

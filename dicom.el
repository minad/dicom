;;; dicom.el --- DICOM viewer -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2024
;; Version: 0.1
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
;; itself in `auto-mode-alist' for DICOMDIR directory files and DICOM images
;; (file extension *.dcm or *.ima).  Furthermore the command `dicom-open' opens
;; DICOMDIR directory files or DICOM image files interactively.

;; The package relies on a few external programs, which are all widely available
;; on Linux distributions.

;; - `convert' from the ImageMagick suite
;; - `ffmpeg' for video conversion
;; - `dcm2xml' from the dcmtk DICOM toolkit
;; - `mpv' for video playing

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

(defvar dicom--timeout 3
  "Timeout for conversion.")

(defvar-local dicom--file nil
  "Current DICOM file open in current buffer.")

(defvar-local dicom--queue nil
  "Conversion process queue in current buffer.")

(defvar-local dicom--proc nil
  "Currently running conversion process in current buffer.")

(defconst dicom--dir-placeholder
  '( :margin 8 :type svg :width 200 :height 200
     :data "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='200'>
<rect width='200' height='200' fill='gray'/></svg>")
  "Placeholder image in `dicom-dir-mode' buffers.")

(defconst dicom--image-placeholder
  (propertize
   " "
   'pointer 'arrow
   'display
   '(image :margin 8 :type svg :width 800 :height 600
           :data "<svg xmlns='http://www.w3.org/2000/svg' width='800' height='600'>
<rect width='800' height='600' fill='gray'/></svg>"))
  "Placeholder image in `dicom-image-mode' buffers.")

(defun dicom--stop (proc)
  "Gracefully stop PROC."
  (ignore-errors (signal-process proc 'TERM))
  (run-at-time 1 nil (lambda () (ignore-errors (delete-process proc)))))

(defun dicom--put-image (pos file)
  "Display image FILE at POS."
  (with-silent-modifications
    (put-text-property pos (1+ pos) 'display `(image :margin 8 :type png :file ,file))))

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
      (insert (format "    %-35s %s\n" k v)))))

(defun dicom--read (file)
  "Read DICOM FILE and return list of items."
  (let ((dom (with-temp-buffer
               (unless (eq 0 (call-process "dcm2xml" nil t nil
                                           "--quiet" "--charset-assume"
                                           "latin-1" "--convert-to-utf8" file))
                 (error "dcm2xml failed"))
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
      (dicom-open file 'reuse)
    (user-error "No image at point")))

(defvar-keymap dicom-dir-mode-map
  :doc "Keymap used by `dicom-dir-mode'."
  :parent special-mode-map
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer
  "RET" #'dicom-open-at-point
  "<mouse-1>" #'dicom-open-at-point)

(defvar-keymap dicom-image-mode-map
  :doc "Keymap used by `dicom-image-mode'."
  :parent special-mode-map
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer)

(define-derived-mode dicom-dir-mode special-mode "DICOM DIR"
  "DICOM DIR mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

(define-derived-mode dicom-image-mode special-mode "DICOM IMAGE"
  "DICOM IMAGE mode."
  :interactive nil :abbrev-table nil :syntax-table nil)

(defun dicom-open (file &optional reuse)
  "Open DICOM dir or image FILE.
If REUSE is non-nil, reuse image buffer."
  (interactive "fDICOM: ")
  (if (or (file-directory-p file) (string-suffix-p "/DICOMDIR" file))
      (let* ((default-directory
              (file-name-as-directory
               (expand-file-name (string-remove-suffix "/DICOMDIR" file))))
             (buf (dicom--buffer-name default-directory)))
        (unless (get-buffer buf)
          (with-current-buffer (get-buffer-create buf)
            (dicom--dir-setup)))
        (pop-to-buffer buf))
    (let* ((file (expand-file-name file))
           (default-directory (file-name-directory file))
           (buf (if reuse "*dicom image*" (dicom--buffer-name file))))
      (unless (when-let ((buf (get-buffer buf)))
                (equal (buffer-local-value 'dicom--file buf) file))
        (with-current-buffer (get-buffer-create buf)
          (dicom--image-setup file)))
      (if reuse
          (display-buffer buf)
        (pop-to-buffer buf)))))

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

(defun dicom--setup-outline ()
  "Setup outline mode."
  (goto-char (point-min))
  (setq-local outline-regexp " [A-Z]"
              outline-minor-mode-cycle t
              outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode))

(defun dicom--play (rate)
  "Play DICOM multi frame image with frame RATE."
  (pcase-let ((`(,dst . ,tmp) (dicom--cache-name dicom--file "mp4")))
    (cond
     ((file-exists-p dst)
      (message "Playing %s…" dicom--file)
      (unless (eq 0 (call-process-shell-command
                     (format "(mpv --loop %s) & disown"
                             (shell-quote-argument dst))
                     nil 0))
        (error "mpv failed")))
     (dicom--proc
      (message "Conversion in progress…"))
     (t
      (message "Converting %s ⟶ %s…" dicom--file dst)
      (let (dicom--timeout)
        (dicom--async (lambda (success)
                        (if success
                            (progn
                              (rename-file tmp dst)
                              (dicom--play rate))
                          (delete-file tmp)))
                      "sh" "-c"
                      (format "convert %s ppm:- | ffmpeg -framerate %s -i - %s"
                              (shell-quote-argument dicom--file)
                              rate
                              (shell-quote-argument tmp))))))))

(defun dicom--image-setup (file)
  "Setup `dicom-image-mode' buffer for image FILE."
  (dicom-image-mode)
  (dicom--stop dicom--proc)
  (setq-local dicom--proc nil
              dicom--file file
              buffer-read-only t
              revert-buffer-function (lambda (&rest _) (dicom--image-setup file))
              header-line-format
              (format
               (propertize " DICOM IMAGE %s"
                           'face '(:inherit header-line :height 1.5 :weight bold))
               (if-let ((dir (locate-dominating-file file "DICOMDIR")))
                   (file-name-sans-extension
                    (file-relative-name file (file-name-parent-directory dir)))
                 (file-name-base file))))
  (with-silent-modifications
    (erase-buffer)
    (insert "\n")
    (pcase-let* ((default-directory "/")
                 (`(,dst . ,tmp) (dicom--cache-name (concat "large" file)))
                 (pos (point))
                 (data (dicom--read dicom--file)))
      (insert dicom--image-placeholder "\n")
      (when (alist-get 'NumberOfFrames (car data))
        (insert-button
         "[PLAY]" 'action
         (lambda (_)
           (dicom--play (or (alist-get 'RecommendedDisplayFrameRate (car data))
                            (alist-get 'CineRate (car data))
                            25))))
        (insert "\n"))
      (mapc #'dicom--insert data)
      (dicom--setup-outline)
      (if (file-exists-p dst)
          (dicom--put-image pos dst)
        (dicom--async (lambda (success)
                        (if success
                         (progn
                           (rename-file tmp dst)
                           (dicom--put-image pos dst))
                       (delete-file tmp)))
                      "convert" file "-delete" "1--1" tmp)))))

(defun dicom--dir-setup ()
  "Setup `dicom-dir-mode' buffer."
  (dicom-dir-mode)
  (dicom--stop dicom--proc)
  (setq-local dicom--queue nil
              dicom--proc nil
              dicom--file (expand-file-name "DICOMDIR")
              buffer-read-only t
              truncate-lines nil
              revert-buffer-function (lambda (&rest _) (dicom--dir-setup))
              fringe-indicator-alist '((continuation . nil)
                                       (truncation . nil))
              header-line-format
              (format (propertize " DICOM DIR %s"
                                  'face '(:inherit header-line :height 1.5 :weight bold))
                      (file-name-base (directory-file-name default-directory))))
  (with-silent-modifications
    (erase-buffer)
    (dolist (item (dicom--read dicom--file))
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
                     " " 'display `(image ,@dicom--dir-placeholder)
                     'pointer 'arrow
                     'dicom--file src
                     'help-echo tooltip))
            (if (file-exists-p dst)
                (dicom--put-image pos dst)
              (push (list pos dst tmp src) dicom--queue))))))
    (dicom--setup-outline)
    (setq dicom--queue (nreverse dicom--queue))
    (dicom--process-queue)))

(defun dicom--buffer-name (file)
  "Buffer name for FILE."
  (format "*dicom: %s*" (file-name-base (directory-file-name file))))

;;;###autoload
(defun dicom-dir-mode-auto ()
  "Enable `dicom-dir-mode' in current buffer."
  (setq-local buffer-file-name nil
              buffer-file-truename nil)
  (rename-buffer (dicom--buffer-name default-directory) t)
  (dicom--dir-setup))

;;;###autoload
(defun dicom-image-mode-auto ()
  "Enable `dicom-image-mode' in current buffer."
  (let ((file (expand-file-name buffer-file-name)))
    (setq-local buffer-file-name nil
                buffer-file-truename nil)
    (rename-buffer (dicom--buffer-name file) t)
    (dicom--image-setup file)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:dcm\\|ima\\)\\'" . dicom-image-mode-auto))

;;;###autoload
(add-to-list 'auto-mode-alist '("DICOMDIR\\'" . dicom-dir-mode-auto))

(provide 'dicom)
;;; dicom.el ends here

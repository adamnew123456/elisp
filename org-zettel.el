;;; -*- lexical-binding: t -*-
;; Copyright 2019, Chris Marchetti

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; org-zettel: A Zettelkasten extension based upon org-mode
;;
;; To start, run org-zettel-init to configure your org-zettel-directory and
;; create the first note.

(require 'org)

(defvar org-zettel-directory "zettel"
  "The name of the subdirectory within your org-directory to put zettel files in")

;; Address utility functions
;;
;; A zettel address is a list of integers, with 1 represenging the first zettel at each level of
;; hierarchy. For example, (1) is the root zettel and (1 2 3) is a grandchild of the root zettel
;; (3rd child of the 2nd child of the root)

(defun org-zettel//down-addr (addr)
  "Gives the address of the first child, relative to the given zettel address"
  (append addr '(1)))

(defun org-zettel//up-addr (addr)
  "Gives the address of the parent, relative to the given zettel address"
  (if (= (length addr) 1)
      nil
    (reverse (cdr (reverse addr)))))

(defun org-zettel//left-addr (addr)
  "Gives the address of the left sibling, relative to the given zettel address"
  (if (= (car (reverse addr)) 1)
      nil
    (let* ((raddr (reverse addr))
           (init-rev (cdr raddr))
           (last-rev (car raddr)))
      (reverse (cons (- last-rev 1) init-rev)))))

(defun org-zettel//right-addr (addr)
  "Gives the address of the right sibling, relative to the given zettel address"
  (let* ((raddr (reverse addr))
         (init-rev (cdr raddr))
         (last-rev (car raddr)))
    (reverse (cons (+ last-rev 1) init-rev))))

;; Filename utility functions
;;
;; All zettels are stored in .org files within org-zettel-directory. Th filename
;; of a zettel is its address rendered with "." separators along with the .org
;; extension. For example, the root is at "1.org" and zettel (1 2 3) is stored
;; in "1.2.3.org"

(defun org-zettel//addr-to-str (addr)
  "Converts a zettel address into a short address string"
  (string-join (mapcar #'number-to-string addr) "."))

(defun org-zettel//str-to-addr (short)
  "Converts a short address string into a zettel address"
  (mapcar #'string-to-number (split-string short "\\.")))

(defun org-zettel//resolve-filename (filename)
  "Converts a zettel filename into a zettel path under org-zettel-directory"
  (concat org-directory "/" org-zettel-directory "/" filename))

(defun org-zettel//addr-to-filename (addr)
  "Converts a zettel address into a filename, including the .org extension."
  (format "%s.org" (org-zettel//addr-to-str addr)))

(defun org-zettel//filename-to-addr (filename)
  "Converts a filename into a zettel address"
  (org-zettel//str-to-addr (replace-regexp-in-string "\\.org$" "" filename)))

(defun org-zettel//addr-to-resolved (addr)
  "Converts a zettel address into a fully resolved filename"
  (org-zettel//resolve-filename (org-zettel//addr-to-filename addr)))

(defun org-zettel//addr-exists-p (addr)
  "Tests whether the zettel at the given address exists"
  (file-exists-p (org-zettel//addr-to-resolved addr)))

(defun org-zettel//addr-next (addr)
  "Determines the address of the next new zettel after this one.

This function prefers to put the next note in the left sibling slot, but if that
slot is occupied then it will move to the first child slot. If that is also
occupied, then it will recusively consider the first child slot (its left sibling,
its first child, etc.)"
  (let ((result nil))
    (while (null result)
      (let ((right (org-zettel//right-addr addr))
            (down (org-zettel//down-addr addr)))
        (cond
         ((not (org-zettel//addr-exists-p right))
          (setq result right))

         ((not (org-zettel//addr-exists-p down))
          (setq result down))

         (t
          (setq addr down)))))

    result))

;; Buffer initialization functions

(defun org-zettel-init ()
  "Creates a zettel directory if necessary, and loads the first file"
  (interactive)
  (if (not (file-directory-p (org-zettel//resolve-filename "")))
      (make-directory (org-zettel//resolve-filename "")))

  (org-zettel//open-buffer '(1)))

(defun org-zettel//init-buffer ()
  "Initializes the buffer's zettel address"
  (setq-local org-zettel-buffer-addr
              (org-zettel//filename-to-addr (file-name-nondirectory (buffer-file-name)))))

(defun org-zettel//open-buffer (addr)
  "Opens the buffer for the given zettel and saves its address"
  (let ((target-file (org-zettel//addr-to-resolved addr)))
    (find-file target-file)
    (with-current-buffer (get-file-buffer target-file)
      (org-zettel//init-buffer))))

(defun org-zettel//check-init ()
  "Ensures that the current buffer has been initialized by org-zettel"
  (if (not (boundp 'org-zettel-buffer-addr))
      (error "Cannot use org-zettel functions until org-zettel//init-buffer has been called")))

(defun org-zettel//description (addr)
  "Gets the description line of the zettel at the given address"
  (if (not (org-zettel//addr-exists-p addr))
      nil
    (with-temp-buffer
      (insert-file-contents (org-zettel//addr-to-resolved addr))
      (car (split-string (buffer-string) "\n" t)))))

;; Zettel movement functions

(defun org-zettel-move-left ()
  "Loads the left sibling of this zettel"
  (interactive)
  (org-zettel//check-init)
  (let ((left (org-zettel//left-addr org-zettel-buffer-addr)))
    (if (or (null left)
            (not (org-zettel//addr-exists-p left)))
        (message "%s" "org-zettel: no left sibling for this buffer")
      (org-zettel//open-buffer left))))

(defun org-zettel-move-right ()
  "Loads the right sibling of this zettel"
  (interactive)
  (org-zettel//check-init)
  (let ((right (org-zettel//right-addr org-zettel-buffer-addr)))
    (if (or (null right)
            (not (org-zettel//addr-exists-p right)))
        (message "%s" "org-zettel: no right sibling for this buffer")
      (org-zettel//open-buffer right))))

(defun org-zettel-move-up ()
  "Loads the parent of this zettel"
  (interactive)
  (org-zettel//check-init)
  (let ((up (org-zettel//up-addr org-zettel-buffer-addr)))
    (if (or (null up)
            (not (org-zettel//addr-exists-p up)))
        (message "%s" "org-zettel: no parent for this buffer")
      (org-zettel//open-buffer up))))

(defun org-zettel-move-down ()
  "Loads the first child of this zettel"
  (interactive)
  (org-zettel//check-init)
  (let ((down (org-zettel//down-addr org-zettel-buffer-addr)))
    (if (or (null down)
            (not (org-zettel//addr-exists-p down)))
        (message "%s" "org-zettel: no first child for this buffer")
      (org-zettel//open-buffer down))))

;; Zettel creation functions
(defun org-zettel-add ()
  "Adds a new zettel, with the address being chosen as per org-zettel//addr-next"
  (interactive)
  (org-zettel//check-init)
  (let ((new-addr (org-zettel//addr-next org-zettel-buffer-addr)))
    (org-zettel//open-buffer new-addr)))

(defun org-zettel-add-down ()
  "Adds a new zettel as a child, without considering any right siblings"
  (interactive)
  (org-zettel//check-init)
  (let ((addr org-zettel-buffer-addr))
    (while (org-zettel//addr-exists-p addr)
      (setq addr (org-zettel//down-addr addr)))
    (org-zettel//open-buffer addr)))

(defun org-zettel-add-right ()
  "Adds a new zettel as a sibling, without considering any children"
  (interactive)
  (org-zettel//check-init)
  (let ((addr org-zettel-buffer-addr))
    (while (org-zettel//addr-exists-p addr)
      (setq addr (org-zettel//right-addr addr)))
    (org-zettel//open-buffer addr)))

;; Org-mode link integration
(defun org-zettel//open-short (str)
  "Opens a zettel by its string address"
  (org-zettel//open-buffer (org-zettel//str-to-addr str)))

(org-link-set-parameters "zettel"
                         :follow #'org-zettel//open-short)

;; Org-mode browsing integration
(defun org-zettel//indent (depth)
  "Prints the indent for the given depth"
  (while (> depth 0)
    (princ " ")
    (setq depth (- depth 1))))

(defun org-zettel//render-addr-family (addr level)
  "Renders a zettel address as well as all of its children and later siblings"
  (while (org-zettel//addr-exists-p addr)
    (let ((down (org-zettel//down-addr addr))
          (right (org-zettel//right-addr addr)))

      (org-zettel//indent level)
      (princ (format "- [[zettel:%s][%s]] %s\n"
                     (org-zettel//addr-to-str addr)
                     (org-zettel//addr-to-str addr)
                     (org-zettel//description addr)))

      (if (org-zettel//addr-exists-p down)
          (org-zettel//render-addr-family down (+ level 1)))

      (setq addr right))))

(defun org-zettel-render ()
  "Renders the structure of the zettel as a tree"
  (interactive)
  (let ((buffer-name (generate-new-buffer "*org-zettel map*")))
    (with-output-to-temp-buffer buffer-name
      (org-zettel//render-addr-family '(1) 0))

    (pop-to-buffer buffer-name nil t)
    (org-mode)))

;; Misc
(defun org-zettel-prompt ()
  "Prompts for a zettel address and opens it"
  (interactive)
  (org-zettel//open-short (read-string "zettel address: ")))

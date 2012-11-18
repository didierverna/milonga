;;; milonga-theme.el --- Tango-based Custom theme for Emacs.

;; Copyright (C) 2012 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>


;;; Commentary


;;; Code:

(require 'cl)

(deftheme milonga
  "Emacs Custom theme based on the Tango palette (light background).
Theme customizations available in the milonga-theme group.")

(defgroup milonga-theme nil
  "The Milonga theme custom group.")

(let ((colors '((yellow-1 . "#fce94f")
		(yellow-2 . "#edd400")
		(yellow-3 . "#c4a000")

		(orange-1 . "#fcaf3e")
		(orange-2 . "#f57900")
		(orange-3 . "#ce5c00")
		(orange-4 . "#b35000")

		(brown-1 . "#e9b96e")
		(brown-2 . "#c17d11")
		(brown-3 . "#8f5902")

		(green-1 . "#8ae234")
		(green-2 . "#73d216")
		(green-3 . "#4e9a06")
		(green-4 . "#346604")

		(blue-0 . "#8cc4ff")
		(blue-1 . "#729fcf")
		(blue-2 . "#3465a4")
		(blue-3 . "#204a87")

		(purple-1 . "#ad7fa8")
		(purple-2 . "#75507b")
		(purple-3 . "#5c3566")

		(red-1 . "#ef2929")
		(red-2 . "#cc0000")
		(red-3 . "#a40000")

		(gray-1 . "#eeeeec")
		(gray-2 . "#d3d7cf")
		(gray-3 . "#babdb6")
		(gray-4 . "#888a85")
		(gray-5 . "#5f615c")
		(gray-6 . "#2e3436")))
      (class '((class color) (min-colors 89)))
      (properties '("foreground" "background" "underline" "overline"))
      (faces
       '((default :inherit (gray-6-foreground gray-1-background))

	 (fringe :inherit gray-2-background)
	 (mode-line :box (:line-width -1 :style released-button)
		    :inherit (gray-6-foreground gray-2-background))
	 (mode-line-inactive :box (:line-width -1 :style released-button)
			     :inherit (gray-6-foreground gray-4-background))

	 (cursor :inherit blue-3-background)
	 (highlight :inherit gray-3-background)
	 (region :inherit gray-3-background)
	 (secondary-selection :inherit blue-0-background)
	 (isearch :inherit (gray-1-foreground orange-3-background))
	 (lazy-highlight :inherit brown-1-background)
	 (trailing-whitespace :inherit red-1-background)

	 (minibuffer-prompt :weight bold :inherit blue-3-foreground)

	 (escape-glyph :inherit red-3-foreground)
	 (error :inherit red-3-foreground)
	 (warning :inherit orange-3-foreground)
	 (success :inherit green-3-foreground)
	 
	 (font-lock-builtin-face :inherit purple-2-foreground)
	 (font-lock-comment-face :slant italic :inherit gray-5-foreground)
	 (font-lock-constant-face :weight bold :inherit blue-3-foreground)
	 (font-lock-function-name-face :inherit red-3-foreground)
	 (font-lock-keyword-face :inherit green-4-foreground)
	 (font-lock-string-face :inherit purple-3-foreground)
	 (font-lock-type-face :inherit blue-3-foreground)
	 (font-lock-variable-name-face :inherit orange-4-foreground)
	 
	 (link :inherit (blue-3-foreground blue-3-underline))
	 (link-visited :inherit (blue-2-foreground blue-2-underline))

	 (gnus-group-news-1 :weight bold :inherit orange-4-foreground)
	 (gnus-group-news-1-empty :inherit orange-4-foreground)
	 (gnus-group-news-2 :weight bold :inherit orange-3-foreground)
	 (gnus-group-news-2-empty :inherit orange-3-foreground)
	 (gnus-group-news-3 :weight bold :inherit orange-2-foreground)
	 (gnus-group-news-3-empty :inherit orange-2-foreground)
	 (gnus-group-news-4 :weight bold :inherit orange-1-foreground)
	 (gnus-group-news-4-empty :inherit orange-1-foreground)
	 (gnus-group-news-5 :weight bold :inherit orange-1-foreground)
	 (gnus-group-news-5-empty :inherit orange-1-foreground)
	 (gnus-group-news-6 :weight bold :inherit orange-1-foreground)
	 (gnus-group-news-6-empty :inherit orange-1-foreground)
	 (gnus-group-news-low :weight bold :inherit orange-1-foreground)
	 (gnus-group-news-low-empty :inherit orange-1-foreground)
	 (gnus-group-mail-1 :weight bold :inherit blue-3-foreground)
	 (gnus-group-mail-1-empty :inherit blue-3-foreground)
	 (gnus-group-mail-2 :weight bold :inherit blue-2-foreground)
	 (gnus-group-mail-2-empty :inherit blue-2-foreground)
	 (gnus-group-mail-3 :weight bold :inherit blue-1-foreground)
	 (gnus-group-mail-3-empty :inherit blue-1-foreground)
	 (gnus-group-mail-low :weight bold :inherit blue-0-foreground)
	 (gnus-group-mail-low-empty :inherit blue-0-foreground)
	 (gnus-header-content :inherit green-3-foreground)
	 (gnus-header-from :weight bold :inherit yellow-3-foreground)
	 (gnus-header-subject :inherit red-3-foreground)
	 (gnus-header-name :inherit blue-3-foreground)
	 (gnus-header-newsgroups :inherit gray-4-foreground)

	 (message-header-name :inherit blue-3-foreground)
	 (message-header-cc :inherit yellow-3-foreground)
	 (message-header-other :inherit brown-2-foreground)
	 (message-header-subject :inherit red-3-foreground)
	 (message-header-to :weight bold :inherit yellow-3-foreground)
	 (message-cited-text :slant italic :inherit gray-5-foreground)
	 (message-separator :weight bold :inherit green-3-foreground)

	 (smerge-refined-change :inherit purple-1-background)

	 (ediff-current-diff-A :inherit blue-1-background)
	 (ediff-fine-diff-A :inherit purple-1-background)
	 (ediff-current-diff-B :inherit yellow-1-background)
	 (ediff-fine-diff-B :inherit orange-1-background)

	 (flyspell-duplicate :inherit orange-1-underline)
	 (flyspell-incorrect :inherit red-1-underline)

	 (semantic-decoration-on-includes :inherit green-4-underline)
	 (semantic-decoration-on-private-members-face
	  :inherit gray-2-background)
	 (semantic-decoration-on-protected-members-face
	  :inherit gray-2-background)
	 (semantic-decoration-on-unknown-includes
	  :inherit brown-3-background)
	 (semantic-decoration-on-unparsed-includes
	  :inherit orange-3-underline)
	 (semantic-tag-boundary-face :inherit blue-1-overline)
	 (semantic-unmatched-syntax-face :inherit red-1-underline))))
  
  ;; 1. Create the core theme faces
  (flet ((milonga-theme-make-faces (color-spec)
	   "Create two Milonga theme faces based on COLOR-SPEC.
COLOR-SPEC is of the form (NAME . COLOR-STRING).
The faces will be named milonga-theme-NAME-foreground/background
and will have COLOR-STRING as foreground/background."
	   (dolist (property properties)
	     (let* ((name-string (concat "milonga-theme-"
					 (symbol-name (car color-spec))
					 "-"
					 property))
		    (name (intern name-string)))
	       (custom-declare-face
		name
		`((,class (,(intern (concat ":" property)) ,(cdr color-spec))))
		(concat "The " (upcase name-string) " face.")
		:group 'milonga-theme)))))
    (dolist (color colors)
      (milonga-theme-make-faces color)))

  ;; 2. Declare all faces based on their specification
  (flet ((milonga-theme-face-spec (face-spec)
	   "Return a Custom face specification from FACE-SPEC.
The specification is suitable to be included in a call to
CUSTOM-THEME-SET-FACES."
	   `(,(car face-spec)
	     ((,class (,@(loop for prop in (cdr face-spec)  by #'cddr
			       for val  in (cddr face-spec) by #'cddr
			       collect prop
			       if (and (symbolp val)
				       (member* (symbol-name val) colors
						:key (lambda (elt)
						       (symbol-name (car elt)))
						:test (lambda (elt1 elt2)
							(string-prefix-p 
							 elt2 elt1))))
			         collect (intern (concat "milonga-theme-"
							 (symbol-name val)))
			       else
			         collect val)))))))
    (apply #'custom-theme-set-faces
	   'milonga
	   (mapcar #'milonga-theme-face-spec faces)))

  ;; 3. Create the ANSI color names vector
  (custom-theme-set-variables
   'milonga
   `(ansi-color-names-vector
     (map 'vector
	  (lambda (color)
	    (cdr (member color colors)))
	  '(gray-6 red-3 green-3 yellow-3 blue-3 purple-3 blue-1 gray-1)))))

(provide-theme 'milonga)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; milonga-theme.el ends here

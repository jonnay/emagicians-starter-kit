
(defconst emagician/version "0.2.1")

(setq initial-scratch-message
      (concat ";; -|-+-|- Invocation Failure.  Sorry." "\n"
              ";; You know what to do." "\n"
              ";; \n"
              ";; Emagician Starter kit Version: " emagician/version "\n"
              ";; \n"
              ";;;;;;;;;;;;;;;;;;;;;;;;;;" "\n"
              ";;                      ;;" "\n"      
              ";; It's dangerous to go ;;" "\n"
              ";; alone! take this:    ;;" "\n"
              ";;                      ;;" "\n"
              ";;    ♨   (°-°)   ♨    ;;" "\n"
              ";;                      ;;" "\n"
              ";;;;;;;;;;;;  ;;;;;;;;;;;;" "\n"
              "\n"
              "(shell-command \"emacs --debug-init\")\n"))

(setq inhibit-splash-screen t)

(defun emagician/show-scratch ()
  "Show the scratch buffer"
  (interactive)
  (set-window-buffer nil "*scratch*"))

(add-hook 'emacs-startup-hook 'emagician/show-scratch t)

(defun emagician-reload ()
  "Reload the init file"
  (interactive)
  (org-babel-load-file (expand-file-name "Emagician.org" emagician-dir)))

(add-to-list 'load-path emagician-dir)
(setq custom-file (concat emagician-dir "custom.el"))
(add-to-list 'load-path (concat emagician-dir "src"))
(add-to-list 'load-path (concat emagician-dir "dist"))

(setq initial-scratch-message 
      (concat initial-scratch-message
              "\n"
              "(find-file-other-window emagician-dir)" "\n"
              "(find-file-other-window (concat emagician-dir \"*.org\") t)" "\n"))

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(setq package-user-dir (concat emagician-dir "elpa"))

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("MELPA"       . "http://melpa.milkbox.net/packages/")
        ("org"         . "http://orgmode.org/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar emagician--starter-kit-depth 0)
(defun emagician-starter-kit-load (file &optional header-or-tag)
  "Load configuration from other .org files.
If the optional argument is the id of a subtree then only
configuration from within that subtree will be loaded.  If it is
not an id then it will be interpreted as a tag, and only subtrees
marked with the given tag will be loaded.

For example, to load all of lisp.org simply
add (emagician-starter-kit-load \"lisp\") to your configuration.

To load only the 'window-system' config from
emagician-starter-kit-misc-recommended.org add
 (emagican-starter-kit-load \"misc-recommended\" \"window-system\")
to your configuration."

  (let ((file (expand-file-name (if (string-match ".+\.org" file)
                                    file
                                  (format "%s.org" file))
                                emagician-dir))
        (load-result nil))
    (if (file-exists-p file)
        (progn
          (setq initial-scratch-message 
                (concat initial-scratch-message "\n"
                        ";; Loading " file))
          (setq load-result
                (org-babel-load-file
                 (if header-or-tag
                     (let* ((base (file-name-nondirectory file))
                            (dir  (file-name-directory file))
                            (partial-file (expand-file-name
                                           (concat "." (file-name-sans-extension base)
                                                   ".part." header-or-tag ".org")
                                           dir)))
                       (unless (file-exists-p partial-file)
                         (with-temp-file partial-file
                           (insert
                            (with-temp-buffer
                              (insert-file-contents file)
                              (save-excursion
                                (condition-case nil ;; collect as a header
                                    (progn
                                      (org-link-search (concat"#"header-or-tag))
                                      (org-narrow-to-subtree)
                                      (buffer-string))
                                  (error ;; collect all entries with as tags
                                   (let (body)
                                     (org-map-entries
                                      (lambda ()
                                        (save-restriction
                                          (org-narrow-to-subtree)
                                          (setq body (concat body "\n" (buffer-string)))))
                                      header-or-tag)
                                     body))))))))
                       partial-file)
                   file))))
      (progn 
        (setq initial-scratch-message
              (concat initial-scratch-message "\n"
                      ";; Skipped Loading " file " It doesn't exist!"))
        (setq load-result nil)))
    (setq initial-scratch-message 
          (concat initial-scratch-message
                  "...done!" "\n"))
    load-result))

(when nil
  (flet ((sk-load (base)
           (let* ((path          (expand-file-name base emagician-dir))
                  (literate      (concat path ".org"))
                  (encrypted-org (concat path ".org.gpg"))
                  (plain         (concat path ".el"))
                  (encrypted-el  (concat path ".el.gpg")))
             (cond
              ((file-exists-p encrypted-org) (org-babel-load-file encrypted-org))
              ((file-exists-p encrypted-el)  (load encrypted-el))
              ((file-exists-p literate)      (org-babel-load-file literate))
              ((file-exists-p plain)         (load plain)))))
         (remove-extension (name)
           (string-match "\\(.*?\\)\.\\(org\\(\\.el\\)?\\|el\\)\\(\\.gpg\\)?$" name)
           (match-string 1 name)))
    (let ((elisp-dir (expand-file-name "src" emagician-dir))
          (user-dir (expand-file-name user-login-name emagician-dir)))
      ;; add the src directory to the load path
      (add-to-list 'load-path elisp-dir)
      ;; load specific files
      (when (file-exists-p elisp-dir)
        (let ((default-directory elisp-dir))
          (normal-top-level-add-subdirs-to-load-path)))
      ;; load system-specific config
      (sk-load system-name)
      ;; load user-specific config
      (sk-load user-login-name)
      ;; load any files in the user's directory
      (when (file-exists-p user-dir)
        (add-to-list 'load-path user-dir)
        (mapc #'sk-load
              (remove-duplicates
               (mapcar #'remove-extension
                       (directory-files user-dir t ".*\.\\(org\\|el\\)\\(\\.gpg\\)?$"))
               :test #'string=)))))
)

(load custom-file 'noerror)

(defmacro emagician/defhook (name hook &rest b)
  (let* ((docp (stringp (car b)))
         (body (if docp (cdr b) b)))
    `(progn 
       (defun ,name () 
         ,(concat (if docp (car b) "Not Documented\n") "\nEmagically defined with emagician/defhook.")
         ,@body)
       (add-hook (quote ,hook) (quote ,name)))))

(defmacro emagician-minor-in-major-mode (major-mode minor-mode)
    (let ((turn-on-symbol (intern (concat "turn-on-" (symbol-name minor-mode)))))
      (list
       'progn 
       (when (not (fboundp turn-on-symbol))
         `(defun ,turn-on-symbol ()
            "Automagickally generated by emagicians starter kit."
            (,minor-mode +1)))
       `(add-hook ,major-mode ,minor-mode))))

(ert-deftest emagician-test-minor-in-major-mode ()
  "emagician-minor-in-major macro test"
  (should (equal (macroexpand '(emagician-minor-in-major-mode elisp-mode paredit-mode))
                 '(progn (defun turn-on-paredit-mode "Automagickally generated by emagicians starter kit." (paredit-mode +1))
                         (add-hook elisp-mode paredit-mode)))))

(defmacro emagician/define-mode-key (mode-name key command)
      (let ((define-key-fname (make-symbol (concat (symbol-name mode-name)
                                                   "-key-<"
                                                   (replace-regexp-in-string "\s"
                                                                             "_"
                                                                             (key-description key))
                                                   ">-"
                                                   (symbol-name command)
                                                   "-hook"))))
        `(progn
           (defun ,define-key-fname ()
             ,(concat "Automatically generated hook function. Binds " key " to " (symbol-name command))
             (define-key ,(make-symbol (concat (symbol-name mode-name) "-map")) ,key ,command))
           (add-hook (quote ,(make-symbol (concat (symbol-name mode-name) "-hook"))) 
                     (quote ,define-key-fname)))))
  
  (when nil
    (pp (macroexpand '(emagician/define-mode-key org-mode (kbd "C-SPC") foo)))
(progn
  (defun org-mode-key-<<kbd>_C-SPC>-foo-hook nil "Automatically generated hook function by Emagician/Starter/kit"
        (define-key org-mode-map
          (kbd "C-SPC")
          foo))
  (add-hook 'org-mode-hook 'org-mode-key-<<kbd>_C-SPC>-foo-hook))

"(progn
  (defun org-mode-key-<<kbd>_C-SPC>-foo-hook nil \"Automatically generated hook function\"
        (define-key org-mode-map
          (kbd \"C-SPC\")
          foo))
  (add-hook 'org-mode-hook 'org-mode-key-<<kbd>_C-SPC>-foo-hook))
" 
  )

(defun emagician-expect-package (package)
  "If the named PACKAGE isn't currently installed, install it"
  (unless (package-installed-p package)
    (package-install package)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(emagician/define-mode-key prog "RET" newline-and-indent)

(add-hook 'prog-mode-hook 'linum-mode)

(show-paren-mode)

(setq semantic-default-submodes 
      (append semantic-default-submodes
              '(global-semantic-idle-summary-mode
                global-semantic-idle-completions-mode
                global-semantic-idle-scheduler-mode
                global-semantic-decoration-mode
                global-semantic-hilight-func-mode
                global-semantic-stickyfuc-mode)))

;; Enable Semantic
(semantic-mode 1)

(which-function-mode t)

(setq flymake-no-changes-timeout 2.5)

(emagician-starter-kit-load (concat emagician-dir "Emagician-Jonnay"))

(emagician-starter-kit-load emagician-truename)

(emagician-starter-kit-load (replace-regexp-in-string "/" 
                                                 "-" 
                                                 (symbol-name system-type)))

(emagician-starter-kit-load system-name)

(emagician-starter-kit-load user-login-name)

(setq initial-scratch-message
      (concat ";;; -|-+-|- Sekrut Alien Technology -|-+-|-" "\n"
              ";;; Hail Eris.  All Hail Discordia." "\n"
              ";;;\n"
              ";;; Emacs Vesrion: " emacs-version "\n" 
              ";;; Emagician Starter kit Version: " emagician/version "\n"
              ";;; " "\n"
              (format ";;; Your startup took approximately %.2f seconds" 
                      (- (float-time after-init-time)
                         (float-time before-init-time))) "\n"                                                      
              ";;; -|-+-|- Sekrut Alien Technology -|-+-|-" "\n"
              ";;;\n"
               (mapconcat (lambda (line) 
                            (concat ";;; " line "\n")) 
                          (split-string (cookie (concat emagician-dir 
                                                        "assets/collected-chaos.lines")
                                                ""
                                                "")
                                        "\n")

                          "")
              ))

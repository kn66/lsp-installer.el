;;; lsp-installer.el --- Language server installer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, lsp
;; URL: https://github.com/kn66/lsp-installer

;;; Commentary:

;; This package provides functionality to install, update, and manage
;; language servers for Emacs, similar to vim-lsp-settings.
;;
;; Language servers are installed to ~/.emacs.d/language-servers/
;; and their paths are automatically added to exec-path.
;;
;; Features:
;; - Unified configuration format for all servers
;; - Multiple installation methods: npm, pip, go, gem, github, binary, dotnet, coursier
;; - Simple error handling and validation
;; - Interactive commands with completion
;; - Automatic path management
;;
;; Usage:
;; (require 'lsp-installer)
;; (lsp-installer-setup)  ; Add installed servers to exec-path
;;
;; Interactive commands:
;; - `lsp-installer-install-server'   - Install a server
;; - `lsp-installer-uninstall-server' - Remove a server
;; - `lsp-installer-update-server'    - Update a server
;; - `lsp-installer-update-all-servers' - Update all installed servers
;; - `lsp-installer-list-servers'     - Show available/installed servers

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

(defgroup lsp-installer nil
  "Language server installer for Emacs."
  :group 'tools
  :prefix "lsp-installer-")

(defcustom lsp-installer-install-dir
  (expand-file-name "language-servers" user-emacs-directory)
  "Directory where language servers will be installed."
  :type 'directory
  :group 'lsp-installer)

(defcustom lsp-installer-servers-file
  (expand-file-name "servers.eld"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "File containing server configuration data."
  :type 'file
  :group 'lsp-installer)

;; Executable paths - simplified configuration
(defvar lsp-installer--executables
  '((npm . "npm")
    (pip . "pip3")
    (go . "go")
    (dotnet . "dotnet")
    (gem . "gem")
    (curl . "curl")
    (tar . "tar")
    (java . "java"))
  "Alist of executable names.")

;;; Variables

(defvar lsp-installer--servers-cache nil
  "Cache for loaded server configurations.")

;;; Utility functions

(defun lsp-installer--msg (format-string &rest args)
  "Display a message with LSP-INSTALLER prefix."
  (message "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--err (format-string &rest args)
  "Signal an error with LSP-INSTALLER prefix."
  (error "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--executable-find (exe-key)
  "Find executable for EXE-KEY from our configuration."
  (let ((exe-name (cdr (assq exe-key lsp-installer--executables))))
    (or (executable-find exe-name)
        (when (file-executable-p exe-name)
          exe-name))))

(defun lsp-installer--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun lsp-installer--make-executable (file-path)
  "Make FILE-PATH executable."
  (when (file-exists-p file-path)
    (set-file-modes file-path (logior (file-modes file-path) #o111))))

(defun lsp-installer--get-server-install-dir (server-name)
  "Get installation directory for SERVER-NAME."
  (expand-file-name server-name lsp-installer-install-dir))

(defun lsp-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed."
  (let ((server-dir
         (lsp-installer--get-server-install-dir server-name)))
    (and (file-directory-p server-dir)
         (directory-files server-dir nil "^[^.]"))))


;;; Configuration management

(defun lsp-installer--load-config ()
  "Load server configurations from file."
  (unless lsp-installer--servers-cache
    (when (file-exists-p lsp-installer-servers-file)
      (with-temp-buffer
        (insert-file-contents lsp-installer-servers-file)
        (goto-char (point-min))
        (setq lsp-installer--servers-cache (read (current-buffer))))))
  lsp-installer--servers-cache)

;;;###autoload
(defun lsp-installer-reload-config ()
  "Reload server configuration from file."
  (interactive)
  (setq lsp-installer--servers-cache nil)
  (lsp-installer--load-config)
  (lsp-installer--msg "Configuration reloaded from %s"
                      lsp-installer-servers-file))

(defun lsp-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME."
  (let ((servers (lsp-installer--load-config)))
    (cl-find-if
     (lambda (server)
       (string= (plist-get server :name) server-name))
     servers)))

(defun lsp-installer--list-available-servers ()
  "List all available servers from configuration."
  (let ((servers (lsp-installer--load-config)))
    (mapcar (lambda (server) (plist-get server :name)) servers)))

(defun lsp-installer--list-installed-servers ()
  "List all installed servers."
  (when (file-directory-p lsp-installer-install-dir)
    (cl-remove-if-not
     (lambda (dir)
       (file-directory-p
        (expand-file-name dir lsp-installer-install-dir)))
     (directory-files lsp-installer-install-dir nil "^[^.]"))))

(defun lsp-installer--validate-config (server-name config)
  "Validate server configuration CONFIG for SERVER-NAME."
  (unless config
    (lsp-installer--err "Server %s not found" server-name))
  (let ((method (plist-get config :install-method))
        (source (plist-get config :source))
        (executable (plist-get config :executable))
        (path-dirs (plist-get config :path-dirs)))
    (unless (and method source executable path-dirs)
      (lsp-installer--err
       "Server %s: incomplete configuration (missing method, source, executable, or path-dirs)"
       server-name))
    (unless (member
             method
             '("npm"
               "pip"
               "go"
               "dotnet"
               "gem"
               "coursier"
               "binary"
               "github"))
      (lsp-installer--err "Server %s: unsupported method %s"
                          server-name
                          method))
    ;; Validate :path-dirs - now required and must be a list of strings
    (unless (and (listp path-dirs) (cl-every #'stringp path-dirs))
      (lsp-installer--err
       "Server %s: :path-dirs is required and must be a list of strings"
       server-name))
    t))


;;; Path management

(defun lsp-installer--expand-path-dirs (server-dir path-dirs)
  "Expand PATH-DIRS with wildcard support relative to SERVER-DIR."
  (let ((expanded-paths '()))
    (dolist (path path-dirs)
      (let ((full-pattern (expand-file-name path server-dir)))
        (if (string-match-p "[*?]" path)
            ;; Pattern contains wildcards - expand them
            (let ((matches (file-expand-wildcards full-pattern)))
              (dolist (match matches)
                (when (file-directory-p match)
                  (push match expanded-paths))))
          ;; Regular path - add if directory exists
          (when (file-directory-p full-pattern)
            (push full-pattern expanded-paths)))))
    (nreverse expanded-paths)))

(defun lsp-installer--add-to-exec-path (server-name)
  "Add SERVER-NAME's bin directories to exec-path."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (config (lsp-installer--get-server-config server-name))
         (path-dirs (plist-get config :path-dirs))
         (bin-paths
          (lsp-installer--expand-path-dirs server-dir path-dirs))
         (added 0))
    (dolist (path bin-paths)
      (when (and (file-directory-p path)
                 (not (member path exec-path)))
        (add-to-list 'exec-path path)
        (cl-incf added)))
    (when (> added 0)
      (lsp-installer--msg "Added %d path(s) for %s"
                          added
                          server-name))))

(defun lsp-installer--remove-from-exec-path (server-name)
  "Remove SERVER-NAME's bin directories from exec-path."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (config (lsp-installer--get-server-config server-name))
         (path-dirs (plist-get config :path-dirs))
         (bin-paths
          (lsp-installer--expand-path-dirs server-dir path-dirs))
         (removed 0))
    (dolist (path bin-paths)
      (when (member path exec-path)
        (setq exec-path (remove path exec-path))
        (cl-incf removed)))
    (when (> removed 0)
      (lsp-installer--msg "Removed %d path(s) for %s"
                          removed
                          server-name))))

;;; Installation methods - consolidated and simplified

(defun lsp-installer--run-command (program args &optional cwd)
  "Run PROGRAM with ARGS in CWD. Return exit code."
  (let ((default-directory (or cwd default-directory)))
    (apply #'call-process program nil "*lsp-installer*" t args)))

(defun lsp-installer--install-npm (server-name source)
  "Install npm package SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (npm-exe (lsp-installer--executable-find 'npm)))
    (unless npm-exe
      (lsp-installer--err "npm not found in PATH"))
    (lsp-installer--ensure-directory server-dir)
    (unless (file-exists-p
             (expand-file-name "package.json" server-dir))
      (with-temp-file (expand-file-name "package.json" server-dir)
        (insert
         (json-encode '((name . "lsp-server") (version . "1.0.0"))))))
    (let ((exit-code
           (lsp-installer--run-command npm-exe
                                       (list "install" source)
                                       server-dir)))
      (unless (= exit-code 0)
        (lsp-installer--err "npm install failed (exit code: %d)"
                            exit-code)))))

(defun lsp-installer--install-pip (server-name source)
  "Install pip package SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (venv-dir (expand-file-name "venv" server-dir))
         (python-exe
          (or (executable-find "python3")
              (executable-find "python"))))
    (unless python-exe
      (lsp-installer--err "python not found in PATH"))
    (lsp-installer--ensure-directory server-dir)
    (let ((exit-code
           (lsp-installer--run-command
            python-exe (list "-m" "venv" venv-dir))))
      (unless (= exit-code 0)
        (lsp-installer--err
         "Failed to create virtual environment (exit code: %d)"
         exit-code)))
    (let* ((venv-pip
            (expand-file-name (if (eq system-type 'windows-nt)
                                  "Scripts/pip.exe"
                                "bin/pip")
                              venv-dir))
           (exit-code
            (lsp-installer--run-command
             venv-pip (list "install" source))))
      (unless (= exit-code 0)
        (lsp-installer--err "pip install failed (exit code: %d)"
                            exit-code)))))

(defun lsp-installer--install-go (server-name source)
  "Install Go binary SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (go-exe (lsp-installer--executable-find 'go))
         (process-environment (copy-sequence process-environment)))
    (unless go-exe
      (lsp-installer--err "Go not found in PATH"))
    (lsp-installer--ensure-directory bin-dir)
    (setenv "GOPATH" (expand-file-name "go" server-dir))
    (setenv "GOBIN" bin-dir)
    (let ((exit-code
           (lsp-installer--run-command
            go-exe (list "install" source))))
      (unless (= exit-code 0)
        (lsp-installer--err "go install failed (exit code: %d)"
                            exit-code)))))

(defun lsp-installer--install-gem (server-name source)
  "Install Ruby gem SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (gem-exe (lsp-installer--executable-find 'gem)))
    (unless gem-exe
      (lsp-installer--err "gem not found in PATH"))
    (lsp-installer--ensure-directory bin-dir)
    (let ((exit-code
           (lsp-installer--run-command
            gem-exe
            (list
             "install"
             source
             "--install-dir"
             server-dir
             "--bindir"
             bin-dir))))
      (unless (= exit-code 0)
        (lsp-installer--err "gem install failed (exit code: %d)"
                            exit-code)))))

(defun lsp-installer--install-dotnet (server-name source)
  "Install .NET tool SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir))
         (dotnet-exe (lsp-installer--executable-find 'dotnet)))
    (unless dotnet-exe
      (lsp-installer--err ".NET SDK not found in PATH"))
    (lsp-installer--ensure-directory tools-dir)
    (let ((exit-code
           (lsp-installer--run-command
            dotnet-exe
            (list "tool" "install" source "--tool-path" tools-dir))))
      (unless (= exit-code 0)
        (lsp-installer--err
         "dotnet tool install failed (exit code: %d)"
         exit-code)))))

(defun lsp-installer--install-coursier (server-name source)
  "Install Coursier package SOURCE for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (coursier-exe (or (executable-find "coursier") "coursier"))
         (process-environment (copy-sequence process-environment)))
    (unless (executable-find coursier-exe)
      (lsp-installer--err "coursier not found in PATH"))
    (lsp-installer--ensure-directory bin-dir)
    (setenv "COURSIER_BIN_DIR" bin-dir)
    (let ((exit-code
           (lsp-installer--run-command
            coursier-exe (list "install" source))))
      (unless (= exit-code 0)
        (lsp-installer--err "coursier install failed (exit code: %d)"
                            exit-code)))))

;;; Download and extraction utilities

(defun lsp-installer--download-file (url target-file)
  "Download file from URL to TARGET-FILE."
  (lsp-installer--ensure-directory (file-name-directory target-file))
  (let ((curl-exe (lsp-installer--executable-find 'curl)))
    (unless curl-exe
      (lsp-installer--err "curl not found in PATH"))
    (let ((exit-code
           (lsp-installer--run-command
            curl-exe
            (list "-L" "-f" "--create-dirs" "-o" target-file url))))
      (unless (= exit-code 0)
        (lsp-installer--err "curl download failed (exit code: %d)"
                            exit-code)))))

(defun lsp-installer--extract-archive
    (archive target-dir &optional strip-components)
  "Extract ARCHIVE to TARGET-DIR with optional STRIP-COMPONENTS."
  (lsp-installer--ensure-directory target-dir)
  (cond
   ;; ZIP files
   ((string-match-p "\\.zip\\'" archive)
    (let ((unzip-exe
           (or (executable-find "unzip") (executable-find "7z"))))
      (unless unzip-exe
        (lsp-installer--err "unzip or 7z not found"))
      (let* ((is-7z
              (string-match-p
               "7z" (file-name-nondirectory (or unzip-exe ""))))
             (cmd
              (if is-7z
                  (list "x" archive (concat "-o" target-dir))
                (list "-d" target-dir archive)))
             (exit-code (lsp-installer--run-command unzip-exe cmd)))
        (unless (= exit-code 0)
          (lsp-installer--err
           "unzip extraction failed (exit code: %d)"
           exit-code)))))
   ;; TAR archives
   ((or (string-match-p "\\.tar\\.gz\\'" archive)
        (string-match-p "\\.tar\\.xz\\'" archive)
        (string-match-p "\\.tgz\\'" archive))
    (let* ((is-xz (string-match-p "xz" archive))
           (compression
            (if is-xz
                "J"
              "z"))
           (tar-exe (lsp-installer--executable-find 'tar))
           (args
            (list
             (concat "-x" compression "f") archive "-C" target-dir)))
      (unless tar-exe
        (lsp-installer--err "tar not found"))
      (when strip-components
        (setq args
              (append
               args
               (list
                "--strip-components"
                (number-to-string strip-components)))))
      (let ((exit-code (lsp-installer--run-command tar-exe args)))
        (unless (= exit-code 0)
          (lsp-installer--err "tar extraction failed (exit code: %d)"
                              exit-code)))))
   (t
    (lsp-installer--err "Unsupported archive format: %s" archive))))

;;; Binary installation with simplified selection

(defun lsp-installer--score-asset (asset-name server-name)
  "Score ASSET-NAME for SERVER-NAME based on platform compatibility."
  (let ((score 0)
        (name (downcase asset-name)))
    ;; OS scoring
    (cond
     ((eq system-type 'windows-nt)
      (if (string-match-p "win\\|windows\\|mingw" name)
          (cl-incf score 10)))
     ((eq system-type 'darwin)
      (if (string-match-p "osx\\|darwin\\|mac" name)
          (cl-incf score 10)))
     ((eq system-type 'gnu/linux)
      (if (string-match-p "linux" name)
          (cl-incf score 10))))
    ;; Architecture scoring
    (cond
     ((string-match "x86_64\\|amd64" system-configuration)
      (if (string-match-p "x64\\|x86_64\\|amd64" name)
          (cl-incf score 5)))
     ((string-match "aarch64\\|arm64" system-configuration)
      (if (string-match-p "arm64\\|aarch64" name)
          (cl-incf score 5))))
    ;; Avoid unwanted files
    (when (string-match-p "source\\|debug\\|symbols" name)
      (cl-decf score 20))
    ;; Server-specific scoring
    (when (string= server-name "omnisharp")
      (when (string-match-p "http\\|mono" name)
        (cl-decf score 15)))
    (when (string= server-name "clangd")
      ;; Prefer main clangd package over indexing tools
      (when (string-match-p "indexing.tools" name)
        (cl-decf score 30)))
    score))

(defun lsp-installer--install-github
    (server-name repo-path executable &optional options)
  "Install binary from GitHub release for SERVER-NAME."
  (let* ((api-url
          (format "https://api.github.com/repos/%s/releases/latest"
                  repo-path))
         (temp-buffer (url-retrieve-synchronously api-url t)))
    (unless temp-buffer
      (lsp-installer--err "Failed to fetch GitHub API for %s"
                          repo-path))
    (with-current-buffer temp-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (let* ((release-data (json-read))
             (assets (cdr (assq 'assets release-data)))
             ;; Find best asset using scoring
             (best-asset
              (when (and assets (> (length assets) 0))
                (cl-reduce
                 (lambda (a b)
                   (if (> (lsp-installer--score-asset
                           (cdr (assq 'name a)) server-name)
                          (lsp-installer--score-asset
                           (cdr (assq 'name b)) server-name))
                       a
                     b))
                 (append assets nil)))))
        (kill-buffer)
        (unless best-asset
          (lsp-installer--err "No suitable asset found for %s"
                              server-name))
        (lsp-installer--msg "Selected asset: %s"
                            (cdr (assq 'name best-asset)))
        (lsp-installer--install-binary server-name
                                       (cdr
                                        (assq
                                         'browser_download_url
                                         best-asset))
                                       executable
                                       options)))))

(defun lsp-installer--install-binary
    (server-name url executable &optional options)
  "Install binary from URL for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (temp-dir (make-temp-file "lsp-installer-" t))
         (filename
          (file-name-nondirectory (car (split-string url "?"))))
         (temp-file (expand-file-name filename temp-dir))
         (target-subdir (plist-get options :target-subdir))
         (strip-components (plist-get options :strip-components))
         (extract-dir
          (if target-subdir
              (expand-file-name target-subdir server-dir)
            server-dir)))
    (unwind-protect
        (progn
          (lsp-installer--ensure-directory server-dir)
          (lsp-installer--download-file url temp-file)
          (if (or (string-match-p "\\.tar\\.gz\\'" filename)
                  (string-match-p "\\.tar\\.xz\\'" filename)
                  (string-match-p "\\.tgz\\'" filename)
                  (string-match-p "\\.zip\\'" filename))
              ;; Archive - extract it and preserve structure
              (progn
                (lsp-installer--extract-archive temp-file extract-dir
                                                strip-components)
                ;; Make executable files executable
                (when executable
                  (let ((exec-path
                         (expand-file-name executable extract-dir)))
                    (when (file-exists-p exec-path)
                      (lsp-installer--make-executable exec-path)))))
            ;; Single file - copy to extract directory with original name
            (let ((target-file
                   (expand-file-name
                    (or (file-name-nondirectory executable) filename)
                    extract-dir)))
              (lsp-installer--ensure-directory extract-dir)
              (copy-file temp-file target-file t)
              (lsp-installer--make-executable target-file))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))


;;; Main installation dispatcher

(defun lsp-installer--dispatch-installation (server-name config)
  "Dispatch installation for SERVER-NAME based on CONFIG."
  (let* ((method (plist-get config :install-method))
         (source (plist-get config :source))
         (executable (plist-get config :executable))
         (options (plist-get config :options)))
    ;; Auto-cleanup: remove existing installation if present
    (when (lsp-installer--server-installed-p server-name)
      (lsp-installer--msg "Removing existing %s installation..."
                          server-name)
      (let ((server-dir
             (lsp-installer--get-server-install-dir server-name)))
        (delete-directory server-dir t)))
    (lsp-installer--msg "Installing %s via %s..." server-name method)
    (condition-case err
        (progn
          (cond
           ((string= method "npm")
            (lsp-installer--install-npm server-name source))
           ((string= method "pip")
            (lsp-installer--install-pip server-name source))
           ((string= method "go")
            (lsp-installer--install-go server-name source))
           ((string= method "gem")
            (lsp-installer--install-gem server-name source))
           ((string= method "coursier")
            (lsp-installer--install-coursier server-name source))
           ((string= method "dotnet")
            (lsp-installer--install-dotnet server-name source))
           ((string= method "github")
            (lsp-installer--install-github
             server-name source executable
             options))
           ((string= method "binary")
            (lsp-installer--install-binary
             server-name source executable
             options))
           (t
            (lsp-installer--err "Unsupported install method: %s"
                                method)))
          (lsp-installer--add-to-exec-path server-name)
          (lsp-installer--msg "Successfully installed %s"
                              server-name))
      (error
       (lsp-installer--err "Failed to install %s: %s"
                           server-name
                           (error-message-string err))))))

;;; Interactive commands

;;;###autoload
(defun lsp-installer-install-server (server-name)
  "Install language server SERVER-NAME."
  (interactive (let ((available-servers
                      (lsp-installer--list-available-servers)))
                 (list
                  (completing-read
                   "Install server: " available-servers
                   nil t nil 'lsp-installer-server-history nil))))
  ;; Debug: show what server name we actually received
  (lsp-installer--msg "Attempting to install server: %s" server-name)
  ;; Validate that the server name is actually in our available list
  (let ((available-servers (lsp-installer--list-available-servers)))
    (unless (member server-name available-servers)
      (lsp-installer--err
       "Server %s is not in the available servers list. Available: %s"
       server-name (mapconcat 'identity available-servers ", "))))
  (let ((config (lsp-installer--get-server-config server-name)))
    (lsp-installer--validate-config server-name config)
    (lsp-installer--dispatch-installation server-name config)))

;;;###autoload
(defun lsp-installer-uninstall-server (server-name)
  "Uninstall language server SERVER-NAME."
  (interactive (list
                (completing-read
                 "Uninstall server: "
                 (lsp-installer--list-installed-servers)
                 nil
                 t
                 nil
                 'lsp-installer-server-history
                 nil)))
  (unless (lsp-installer--server-installed-p server-name)
    (lsp-installer--err "Server %s is not installed" server-name))
  (when (y-or-n-p (format "Really uninstall server %s? " server-name))
    (let ((server-dir
           (lsp-installer--get-server-install-dir server-name)))
      (when (file-directory-p server-dir)
        (lsp-installer--remove-from-exec-path server-name)
        (delete-directory server-dir t)
        (lsp-installer--msg "Successfully uninstalled %s"
                            server-name)))))

;;;###autoload
(defun lsp-installer-update-server (server-name)
  "Update language server SERVER-NAME by reinstalling."
  (interactive (list
                (completing-read
                 "Update server: "
                 (lsp-installer--list-installed-servers)
                 nil
                 t
                 nil
                 'lsp-installer-server-history
                 nil)))
  (let ((config (lsp-installer--get-server-config server-name)))
    (lsp-installer--validate-config server-name config)
    (unless (lsp-installer--server-installed-p server-name)
      (lsp-installer--err "Server %s is not installed" server-name))
    (lsp-installer--msg "Updating %s..." server-name)
    (let ((server-dir
           (lsp-installer--get-server-install-dir server-name)))
      (when (file-directory-p server-dir)
        (delete-directory server-dir t)))
    (lsp-installer--dispatch-installation server-name config)
    (lsp-installer--msg "Successfully updated %s" server-name)))

;;;###autoload
(defun lsp-installer-update-all-servers ()
  "Update all installed language servers."
  (interactive)
  (let ((installed (lsp-installer--list-installed-servers)))
    (if (null installed)
        (lsp-installer--msg "No language servers are installed")
      (when (y-or-n-p
             (format "Update %d installed server(s)? "
                     (length installed)))
        (lsp-installer--msg "Updating %d servers..."
                            (length installed))
        (dolist (server-name installed)
          (condition-case err
              (progn
                (lsp-installer--msg "Updating %s..." server-name)
                (let ((config
                       (lsp-installer--get-server-config
                        server-name)))
                  (lsp-installer--validate-config server-name config)
                  (let ((server-dir
                         (lsp-installer--get-server-install-dir
                          server-name)))
                    (when (file-directory-p server-dir)
                      (delete-directory server-dir t)))
                  (lsp-installer--dispatch-installation
                   server-name config)
                  (lsp-installer--msg "Successfully updated %s"
                                      server-name)))
            (error
             (lsp-installer--msg "Failed to update %s: %s"
                                 server-name
                                 (error-message-string err)))))
        (lsp-installer--msg "Finished updating all servers")))))

;;;###autoload
(defun lsp-installer-list-servers ()
  "List all available and installed language servers."
  (interactive)
  (let ((available (lsp-installer--list-available-servers))
        (installed (lsp-installer--list-installed-servers)))
    (with-current-buffer (get-buffer-create "*LSP Installer*")
      (erase-buffer)
      (insert "Language Server Status\n======================\n\n")
      (insert "Available servers:\n")
      (dolist (server available)
        (insert
         (format "  %s%s\n"
                 server
                 (if (member server installed)
                     " [INSTALLED]"
                   ""))))
      (when installed
        (insert "\nInstalled servers:\n")
        (dolist (server installed)
          (insert (format "  %s\n" server))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun lsp-installer-setup ()
  "Setup language server paths in exec-path."
  (interactive)
  (let ((installed (lsp-installer--list-installed-servers)))
    (if installed
        (progn
          (lsp-installer--msg "Setting up paths for %d servers..."
                              (length installed))
          (dolist (server installed)
            (lsp-installer--add-to-exec-path server))
          (lsp-installer--msg "Language server paths setup complete"))
      (lsp-installer--msg "No language servers installed"))))


(provide 'lsp-installer)

;;; lsp-installer.el ends here

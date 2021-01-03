;; -*- lexical-binding: t -*-

(require 'json)
(require 'url)
(require 'mm-decode)
(require 'cl-lib)

(defgroup wallabag
  '((wallabag-credentials custom-variable)
    (wallabag-data-dir    custom-variable))
  "Customizaiton options for Wallabag.")

(defcustom wallabag-credentials nil
  "Alist containing Wallabag API credentials"
  :type 'sexp)

(defcustom wallabag-data-dir user-emacs-directory
  "Directory to store wallabag token and metadata"
  :type 'directory)

(defun wallabag--get-json-from-response-buffer (buffer)
  (with-current-buffer (car (with-current-buffer buffer (mm-dissect-buffer t)))
    (goto-char (point-min))
    (wallabag--with-json-preset (json-read))))

(defmacro wallabag--with-json-preset (&rest body)
  "Execute BODY with preferred json settings"
  (declare (indent 0) (debug t))
  `(let ((json-key-type 'symbol)
         (json-array-type 'list)
         (json-object-type 'alist))
     ,@body))

(defun wallabag--save-token-payload (token-payload)
  (wallabag--save-data "wallabag-token"
                       `((token . ,(alist-get 'access_token token-payload))
                         (refresh-token . ,(alist-get 'refresh_token token-payload))
                         (expiration-time . ,(+ (float-time) (alist-get 'expires_in token-payload)))
                         (token-type . ,(alist-get 'token_type token-payload)))))

(defun wallabag--save-data (filename data)
  "Save json data to a json file in the data directory"
  (unless (file-directory-p (wallabag--get-data-dir)) (make-directory (wallabag--get-data-dir)))
  (let ((filepath (concat (wallabag--get-data-dir) filename ".json")))
    (with-temp-buffer
      (wallabag--with-json-preset
        (insert (json-encode data))
        (json-pretty-print-buffer)
        (write-file filepath)))))

(defun wallabag--retrieve-and-store-wallabag-token (host client-id client-secret username password)
  (let* ((url-request-data
          (format "grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s"
                  client-id
                  client-secret
                  username
                  password))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (response (url-retrieve-synchronously (concat "https://" host "/oauth/v2/token"))))
    (let ((response-data (wallabag--get-json-from-response-buffer response)))
          (if (alist-get 'error response-data)
              `(wallabag-error ,(format
                        "Got error response from %s: %s: %s"
                        host
                        (alist-get 'error response-data)
                        (alist-get 'error_description response-data)))
            (progn
              (wallabag--save-token-payload response-data)
              '(success))))))

(defun wallabag--refresh-wallabag-token ()
  (interactive)
  (let* ((token (wallabag--get-data "wallabag-token"))
         (wallabag-config wallabag-credentials)
         (url-request-data
          (format "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
                  (alist-get 'refresh-token token)
                  (alist-get 'client-id wallabag-config)
                  (alist-get 'client-secret wallabag-config)))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (response (url-retrieve-synchronously (concat "https://"
                                                       (alist-get 'host wallabag-config)
                                                       "/oauth/v2/token"))))
    (let* ((response-json (wallabag--get-json-from-response-buffer response))
          (response-error (alist-get 'error response-json)))
      (if (and response-error (string= response-error "invalid_grant"))
          (progn
            (message "Failed to retrieve a new wallabag auth token, refresh token is expired.")
            (sleep-for 1)
            (n2wal-replace-wallabag-config)
            (wallabag--refresh-wallabag-token))
        (wallabag--save-token-payload (wallabag--get-json-from-response-buffer response))))))

(defun n2wal-replace-wallabag-config ()
  "Replace the wallabag configuration that is currently active
with a new one that is created interactively."
  (interactive)
  (let* ((config (wallabag--get-data "config"))
         (wallabag-config wallabag-credentials))
    (setcdr wallabag-config
            (wallabag--create-wallabag-config
             (alist-get 'host wallabag-config)
             (alist-get 'client-id wallabag-config)
             (alist-get 'client-secret wallabag-config)
             (alist-get 'username wallabag-config)))

    (wallabag--save-data "config" config)))

(defun wallabag--get-data-dir ()
  "Get the path of the directory where n2wal saves its data"
  (file-name-as-directory wallabag-data-dir))

;; (defun wallabag--create-wallabag-config (&optional host client-id client-secret username password)
;;   "Interactively construct the config parameters for the wallabag connection"
;;   (let* ((config `((host . ,(read-string "Wallabag instance host:" host))
;;                   (client-id . ,(read-string "Wallabag client id:" client-id))
;;                   (client-secret . ,(read-string "Wallabag client secret:" client-secret))
;;                   (username . ,(read-string "Wallabag username:" username))))
;;          (password (read-passwd "Wallabag password:" password))
;;          (token (wallabag--retrieve-and-store-wallabag-token
;;                  (alist-get 'host config)
;;                  (alist-get 'client-id config)
;;                  (alist-get 'client-secret config)
;;                  (alist-get 'username config)
;;                  password)))
;;     (if (not (wallabag--error-p token))
;;         config
;;       (if (string= "yes"
;;                    (cadr
;;                     (read-multiple-choice
;;                      (format "Error: \"%s\" you like to try again? :" (cadr token))
;;                      '((?y "yes") (?n "no")))))
;;           (wallabag--create-wallabag-config
;;            (alist-get 'host config)
;;            (alist-get 'client-id config)
;;            (alist-get 'client-secret config)
;;            (alist-get 'username config)
;;            password)))))

(defun wallabag--error-p (thing)
  (and (listp thing) (eq 'wallabag-error (car thing))))

(defun wallabag--create-wallabag-config (&optional host client-id client-secret username password)
  "Interactively construct the config parameters for the wallabag connection"
  (interactive)
  (let* ((config (append wallabag-credentials 
                         `((username . ,(read-string "Wallabag username: " username)))))
         (password (read-passwd "Wallabag password: " password))
         (token (wallabag--retrieve-and-store-wallabag-token
                 (alist-get 'host config)
                 (alist-get 'client-id config)
                 (alist-get 'client-secret config)
                 (alist-get 'username config)
                 password)))
    (if (not (wallabag--error-p token))
        config
      (if (y-or-n-p (format "Error: \"%s\" you like to try again? :"
                            (cadr token)))
          (wallabag--create-wallabag-config
           (alist-get 'host config)
           (alist-get 'client-id config)
           (alist-get 'client-secret config)
           (alist-get 'username config)
           password)))))

(defun wallabag--get-data (filename)
  "Open a json file in the data directory and return the parsed data"
  (let ((filepath (concat (wallabag--get-data-dir) filename ".json")))
    (if (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (goto-char (point-min))
          (wallabag--with-json-preset
            (json-read))))))

(defun wallabag--make-wallabag-client (host token refresh-token expiration-time)
  (lambda (method route &optional data)
    (if (> (float-time) expiration-time)
        (progn
          (wallabag--refresh-wallabag-token)
          (let ((new-token-data  (wallabag--get-data "wallabag-token")))
            (setq token (alist-get 'token new-token-data))
            (setq refresh-token (alist-get 'refresh-token new-token-data))
            (setq expiration-time (alist-get 'expiration-time new-token-data)))))

    (let ((url-request-method method)
          (url-request-extra-headers
           `(("Accepts" . "application/json")
             ("Content-Type" . "application/json")
             ("Authorization" . ,(format "Bearer %s" token))))
          (url-request-data (if data data)))
      (url-retrieve-synchronously (concat "https://" host "/" route) t))))

(defun wallabag-post-entry (&optional read-url-maybe)
  "Post URL to Wallabag. If no URL is supplied, use URL around
point (if one is found.)"
  (interactive)
  (let* ((read-url (or read-url-maybe
                       (thing-at-point 'url)
                       (error "No URL specified")))
         (token (or (wallabag--get-data "wallabag-token")
                    (progn (wallabag--create-wallabag-config)
                           (wallabag--get-data "wallabag-token"))
                    (error "Could not fetch token")))
         (wallabag-client (wallabag--make-wallabag-client
                           (alist-get 'host wallabag-credentials)
                           (alist-get 'token token)
                           (alist-get 'refresh-token token)
                           (alist-get 'expiration-time token)))
         (response (funcall wallabag-client
                            "POST"
                            "api/entries.json"
                            (wallabag--with-json-preset
                              (json-encode `((url . ,read-url))))))
         (response-data (wallabag--get-json-from-response-buffer response)))
    
    (if (not (alist-get 'id response-data))
        (message "Error while adding article, unable to extract wallabag article id from response: %s"
                 (with-current-buffer response (buffer-string)))
      (message "Posted to Wallabag at %s: %s"
               (alist-get 'host wallabag-credentials)
               read-url))))

(provide 'wallabag)

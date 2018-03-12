;;; csdn-auth.el --- functions to handler auth of csdn

;; Copyright (C) 2004-2016 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-03-24
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((request "0.2.0") (emacs "24.4"))
;; URL: https://github.com/lujun9972/csdn-api.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; csdn-api's code can be found here:
;;   http://github.com/lujun9972/csdn-api.el

;;; Commentary:

;; csdn-api is a wrapper of csdn api

(require 'request)
;;; Code:

(defconst csdn-auth-app-key "1100366" "APP KEY.")
(defconst csdn-auth-app-secret "db897828ed5a44e58943e00aac6c08b0" "APP SECRET.")

(defcustom csdn-auth-username nil
  "授权用户的用户名."
  )

(defcustom csdn-auth-password nil
  "授权用户的密码."
  )

(defun csdn-auth-get-username ()
  "返回授权用户的用户名."
  (or csdn-auth-username
      (setq csdn-auth-username (read-string "请输入登陆用户："))))

(defun csdn-auth-get-password ()
  "返回授权用户的密码."
  (or csdn-auth-password
      (setq csdn-auth-password (read-passwd "请输入登陆密码："))))

(defvar csdn-auth-access-token nil)

(defun csdn-auth-access-token (username password)
  "直接传输用户名、密码来获取Access Token."
  (let ((username (or username (csdn-auth-get-username)))
        (password (or password (csdn-auth-get-password)))
        (grant-type "password")
        (url (format "http://api.csdn.net/oauth2/access_token")))
    (let* ((response (request url
                              :type "GET"
                              :params `(("client_id" . ,csdn-auth-app-key)
                                        ("client_secret" . ,csdn-auth-app-secret)
                                        ("grant_type" . ,grant-type)
                                        ("username" . ,username)
                                        ("password" . ,password))
                              :parser #'json-read
                              :sync t))
           (response-data (request-response-data response))
           (access-token (cdr (assoc-string "access_token" response-data))))
      access-token)))

(defun csdn-auth-clear ()
  "重置Access Token."
  (setq csdn-auth-password nil
        csdn-auth-username nil
        csdn-auth-access-token nil))

(defun csdn-auth-get-access-token ()
  "返回Access Token."
  (or csdn-auth-access-token
      (setq csdn-auth-access-token
            (csdn-auth-access-token (csdn-auth-get-username)
                                    (csdn-auth-get-password)))))

;; (csdn-auth-get-access-token)


(provide 'csdn-auth)

;;; csdn-auth.el ends here

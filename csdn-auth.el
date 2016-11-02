(require 'request)
(defconst csdn-auth-app-key "1100366" "APP KEY")
(defconst csdn-auth-app-secret "db897828ed5a44e58943e00aac6c08b0" "APP SECRET")

(defcustom csdn-auth-username nil
  "授权用户的用户名"
  )

(defcustom csdn-auth-password nil
  "授权用户的密码"
  )

(defun csdn-auth-get-username ()
  "返回授权用户的用户名"
  (or csdn-auth-username
      (setq csdn-auth-username (read-string "请输入登陆用户："))))

(defun csdn-auth-get-password ()
  "返回授权用户的密码"
  (or csdn-auth-password
      (setq csdn-auth-password (read-passwd "请输入登陆密码："))))

(defvar csdn-auth-access-token nil)

(defun csdn-auth-access-token (username password)
  "直接传输用户名、密码来获取Access Token"
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

(defun csdn-auth-get-access-token ()
  "返回Access Token"
  (or csdn-auth-access-token
      (setq csdn-auth-access-token
            (csdn-auth-access-token (csdn-auth-get-username)
                                    (csdn-auth-get-password)))))

;; (csdn-auth-get-access-token)


(provide 'csdn-auth)

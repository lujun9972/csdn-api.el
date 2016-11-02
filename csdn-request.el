(require 'csdn-auth)
(require 'request)

(defun csdn-request-response-data (url &rest url-params-plist)
  "访问csdn api url并返回结果.

URL为csdn api url
URL-PARAMS-PLIST为传递给URL的参数
"
  (let* ((access-token (csdn-auth-get-access-token))
         (url-params-to-data-fn (lambda (plist)
                                  "Return an alist of the property-value pairs in PLIST."
                                  (let (res)
                                    (while plist
                                      (let ((prop (pop plist))
                                            (val (pop plist)))
                                        (when val
                                          (push (cons (substring (format "%s" prop) 1)
                                                      (format "%s" val)) res))))
                                    (nreverse res))))
         (data (funcall url-params-to-data-fn url-params-plist))
         (data (cons `("access_token" . ,access-token) data)))
    (request-response-data (request url
                                    :type "POST"
                                    :data data
                                    :parser (lambda ()
                                              (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))
                                    :sync t))))

(provide 'csdn-request)

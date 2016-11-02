(require 'csdn-auth)
(require 'request)

(defun csdn-request-response-data (url &rest url-params-plist)
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
                                    :parser #'json-read
                                    :sync t))))

(provide 'csdn-request)

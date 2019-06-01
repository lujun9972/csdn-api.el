;;; csdn-request.el --- access the csdn api url and return the result

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
;;   https://github.com/lujun9972/csdn-api.el

;;; Commentary:

;; csdn-api is a wrapper of csdn api

(require 'csdn-auth)
(require 'request)

;;; Code:

(defun csdn-request-response-data (url &rest url-params-plist)
  "访问csdn api url并返回结果.

URL为csdn api url
URL-PARAMS-PLIST为传递给URL的参数"
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

;;; csdn-request.el ends here

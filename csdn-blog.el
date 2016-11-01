(require 'csdn-auth)
(require 'request)

(defun csdn-blog-add-article (title content &optional type description categories tags)
  ""
  (let ((url "http://api.csdn.net/blog/savearticle")
        (type (or type "original"))
        (description (or description (substring content 0 100)))
        (categories (or categories ""))
        (tags (or tags "")))
    (request url
             :type "POST"
             :params `(("access_token" . ,(csdn-auth-get-access-token))
                       ("title" . ,title)
                       ("content" . ,content)
                       ("type" . ,type)
                       ("description" . ,description)
                       ("categories" . ,categories)
                       ("tags" . ,tags)))))

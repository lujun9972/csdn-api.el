(require 'csdn-request)


(defmacro csdn-defun-api (fn url doc &optional required-args-alist optional-args-alist)
  ""
  (declare (debug t) (indent 1))
  (let* ((get-arg-doc-fn (lambda (arg)
                           (let ((arg-name (format "%s" (car arg)))
                                 (arg-desc (cdr arg)))
                             (format "%s: %s" (upcase arg-name) arg-desc))))
         (get-args-doc-fn (lambda (arg-alist)
                            (string-join (mapcar (lambda (arg)
                                                   (funcall get-arg-doc-fn arg))
                                                 arg-alist)
                                         "\n")))
         (get-doc-fn (lambda (doc required-args-alist optional-args-alist)
                       (let ((required-args-doc (funcall get-args-doc-fn required-args-alist))
                             (optional-args-doc (funcall get-args-doc-fn optional-args-alist)))
                         (concat doc "\n\n" required-args-doc "\n" optional-args-doc))))
         (doc (funcall get-doc-fn doc required-args-alist optional-args-alist))

         (required-args (mapcar #'car required-args-alist))
         (optional-args (mapcar #'car optional-args-alist))
         (arg-list (if optional-args
                       `(,@required-args &optional ,@optional-args)
                     required-args))

         (symbol-to-key-fn (lambda (sym)
                             (intern (concat ":" (symbol-name sym)))))
         (symbol-to-property (lambda (sym)
                               (list (funcall symbol-to-key-fn sym)
                                     sym)))
         (param-plist (append (cl-mapcan symbol-to-property required-args)
                              (cl-mapcan symbol-to-property optional-args))))
    `(defun ,fn ,arg-list
       ,doc
       (csdn-request-response-data ,url ,@param-plist))))



;; 用户接口
(csdn-defun-api csdn-user-get-email "http://api.csdn.net/user/getemail" "获取用户的邮箱")
;; (csdn-user-get-email)
(csdn-defun-api csdn-user-get-info "http://api.csdn.net/user/getinfo" "获取用户基本资料")
;; (csdn-user-get-info)
(csdn-defun-api csdn-user-get-mobile "http://api.csdn.net/user/getmobile" "获取用户的手机")
;; (csdn-user-get-mobile)
(csdn-defun-api csdn-user-get-avatar "http://api.csdn.net/user/getavatar" "批量获取用户的头像"
                ((users . "用户名数组（逗号分隔）"))
                ((size . "头像大小（1|2|3）")))
;; (csdn-user-get-avatar)


;; 博客接口
;; "http://api.csdn.net/blog/getinfo"	"获取博主基本信息"
;; "http://api.csdn.net/blog/getstats"	"获取博主的统计信息"
;; "http://api.csdn.net/blog/getmedal"	"获取博主的勋章"
;; "http://api.csdn.net/blog/getcolumn"	"获取博主的专栏"
;; "http://api.csdn.net/blog/getarticlelist"	"获取博主的文章列表"
;; "http://api.csdn.net/blog/getarticle"	"获取文章内容"
;; "http://api.csdn.net/blog/getcategorylist"	"获取博主的自定义分类"
;; "http://api.csdn.net/blog/gettaglist"	"获取博主使用过的的标签"
;; "http://api.csdn.net/blog/getcommentlist"	"获取博主收到的评论"
;; "http://api.csdn.net/blog/getmycommentlist"	"获取博主发出的评论"
;; "http://api.csdn.net/blog/getarticlecomment"	"获取文章的评论"
;; "http://api.csdn.net/blog/saveinfo"	"修改博主信息"
;; "http://api.csdn.net/blog/savearticle"	"发表/修改文章"
;; "http://api.csdn.net/blog/postcomment"	"发表评论"
;; "http://api.csdn.net/blog/getnewarticlelist"	"获取博客最新文章"
;; "http://api.csdn.net/blog/gethomenewest"	"获取首页最新文章"
;; "http://api.csdn.net/blog/getexpertlist"	"获取博客专家"
;; "http://api.csdn.net/blog/getcolumnlist"	"获取专栏列表"
;; "http://api.csdn.net/blog/getcolumndetails"	"获取专栏信息"
;; "http://api.csdn.net/blog/getcolumnarticles"	"获取专栏的文章"
;; "http://api.csdn.net/blog/getchannel"	"获取博客系统分类"


;; 论坛接口
;; "http://api.csdn.net/bbs/gettopics"	"获取板块最新帖子列表"
;; "http://api.csdn.net/bbs/gettopicdetails"	"获取帖子详细内容"
;; "http://api.csdn.net/bbs/posttopic"	"发布帖子"
;; "http://api.csdn.net/bbs/postreply"	"回复帖子"
;; "http://api.csdn.net/bbs/getreplies"	"获取回复列表"
;; "http://api.csdn.net/bbs/getreplyusers"	"获取回复的用户"

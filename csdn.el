;;; csdn.el --- a wrapper of csdn api

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

(require 'csdn-request)

;;; Code:

(defmacro csdn-defun-api (fn url doc &optional required-args-alist optional-args-alist)
  "定义csdn api函数的宏.

FN为定义的函数叫什么名字
URL表示csdn api url
DOC表示csdn api的功能说明
REQUIRED-ARGS-ALIST是必填函数及其说明的alist，其元素格式为 (ARG-NAME . ARG-DESCRIPTION)
OPTIONAL-ARGS-ALIST是可选函数及其说明的alist，其元素格式为 (ARG-NAME . ARG-DESCRIPTION)"
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
(csdn-defun-api csdn-user-get-email "https://api.csdn.net/user/getemail" "获取用户的邮箱")
;; (csdn-user-get-email)

(csdn-defun-api csdn-user-get-info "https://api.csdn.net/user/getinfo" "获取用户基本资料")
;; (csdn-user-get-info)

(csdn-defun-api csdn-user-get-mobile "https://api.csdn.net/user/getmobile" "获取用户的手机")
;; (csdn-user-get-mobile)

(csdn-defun-api csdn-user-get-avatar "https://api.csdn.net/user/getavatar" "批量获取用户的头像"
                ((users . "用户名数组（逗号分隔）"))
                ((size . "头像大小（1|2|3）")))
;; (csdn-user-get-avatar)


;; 博客接口
(csdn-defun-api csdn-blog-get-info  "https://api.csdn.net/blog/getinfo" "获取博主基本信息" )
;; (csdn-blog-get-info)

;; "https://api.csdn.net/blog/getstats"	"获取博主的统计信息"
(csdn-defun-api csdn-blog-get-medal "https://api.csdn.net/blog/getmedal" "获取博主的勋章")
;; (csdn-blog-get-medal)

(csdn-defun-api csdn-blog-get-column  "https://api.csdn.net/blog/getcolumn" "获取博主的专栏")
;; (csdn-blog-get-column)

(csdn-defun-api csdn-blog-get-article-list "https://api.csdn.net/blog/getarticlelist" "获取博主的文章列表"
                nil
                ((status . "文章状态，取值范围：enabled|draft，默认enabled")
                 (page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-article-list)

(csdn-defun-api csdn-blog-get-article  "https://api.csdn.net/blog/getarticle" "获取文章内容"
                ((id . "文章id")))
;; (csdn-blog-get-article 53000412)

(csdn-defun-api csdn-blog-get-category-list "https://api.csdn.net/blog/getcategorylist" "获取博主的自定义分类")
;; (csdn-blog-get-category-list)

(csdn-defun-api csdn-blog-get-tag-list "https://api.csdn.net/blog/gettaglist" "获取博主使用过的的标签")
;; (csdn-blog-get-tag-list)

(csdn-defun-api csdn-blog-get-comment-list "https://api.csdn.net/blog/getcommentlist" "获取博主收到的评论"
                nil
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-comment-list)

(csdn-defun-api csdn-blog-get-my-comment-list "https://api.csdn.net/blog/getmycommentlist" "获取博主发出的评论"
                nil
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-my-comment-list)

(csdn-defun-api csdn-blog-get-article-comment "https://api.csdn.net/blog/getarticlecomment" "获取文章的评论"
                ((article . "文章的id"))
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-article-comment 53000412)

;; "https://api.csdn.net/blog/saveinfo"	"修改博主信息"

(csdn-defun-api csdn-blog-add-article "https://api.csdn.net/blog/savearticle" "发表/修改文章"
                ((title . "文章标题")
                 (content . "文章内容"))
                ((type . "文章类型（original|report|translated）")
                 (description . "文章简介")
                 (categories . "自定义类别（英文逗号分割）")
                 (tags . "文章标签（英文逗号分割）")
                 (ip . "用户ip")))
;; (csdn-blog-add-article "api-title" "api-content")

(csdn-defun-api csdn-blog-modify-article "https://api.csdn.net/blog/savearticle" "发表/修改文章"
                ((id . "文章id")
                 (title . "文章标题")
                 (content . "文章内容"))
                ((type . "文章类型（original|report|translated）")
                 (description . "文章简介")
                 (categories . "自定义类别（英文逗号分割）")
                 (tags . "文章标签（英文逗号分割）")
                 (ip . "用户ip")))
;; (csdn-blog-modify-article 53012169 "new-title" "new-content")

(csdn-defun-api csdn-blog-post-comment "https://api.csdn.net/blog/postcomment" "发表评论"
                ((article . "被评论的文章id")
                 (content . "评论内容"))
                ((reply_id . "被回复的评论id")
                 (ip . "用户ip")))
;; (csdn-blog-post-comment 53012169 "comment-from-api")

(csdn-defun-api csdn-blog-get-new-article-list  "https://api.csdn.net/blog/getnewarticlelist" "获取博客最新文章"
                nil
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-new-article-list)

;; "https://api.csdn.net/blog/gethomenewest"	"获取首页最新文章"

(csdn-defun-api csdn-blog-get-expertlist "https://api.csdn.net/blog/getexpertlist" "获取博客专家"
                nil
                ((channel . "专家类别")))
;; (csdn-blog-get-expertlist)

(csdn-defun-api csdn-blog-get-column-list "https://api.csdn.net/blog/getcolumnlist" "获取专栏列表"
                nil
                ((channel . "专栏类别")
                 (page . "当前页码，默认1")
                 (size . "每页条数，默认15")))
;; (csdn-blog-get-column-list)

;; "https://api.csdn.net/blog/getcolumndetails"	"获取专栏信息"

;; "https://api.csdn.net/blog/getcolumnarticles"	"获取专栏的文章"

(csdn-defun-api csdn-blog-get-channel "https://api.csdn.net/blog/getchannel" "获取博客系统分类")
;; (csdn-blog-get-channel)


;; 论坛接口
(csdn-defun-api csdn-bbs-get-topics "https://api.csdn.net/bbs/gettopics" "获取板块最新帖子列表"
                 ((froum . "板块别名"))) ;这里居然是froum而不是forum，我也是醉了！！
;; (csdn-bbs-get-topics "VB.NET")

(csdn-defun-api csdn-bbs-get-topic-detail  "https://api.csdn.net/bbs/gettopicdetails" "获取帖子详细内容"
                ((id . "帖子ID")))

(csdn-defun-api csdn-bbs-post-topic  "https://api.csdn.net/bbs/posttopic" "发布帖子"
                ((title	. "帖子标题")
                 (content . "帖子内容（bbcode格式）")
                 (forum . "所属板块别名")
                 (point . "悬赏的分数"))
                ((tags . "标签（多个标签英文逗号分隔）")
                 (ip . "发帖人ip，默认为app服务器ip")))

(csdn-defun-api csdn-bbs-post-reply "https://api.csdn.net/bbs/postreply" "回复帖子"
                ((id . "帖子ID")
                 (content . "	回复的内容（bbcode格式）"))
                ((ip . "回复人ip，默认为app服务器ip")))

(csdn-defun-api csdn-bbs-get-replies "https://api.csdn.net/bbs/getreplies" "获取回复列表"
                ((id . "帖子id"))
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))

(csdn-defun-api csdn-bbs-get-reply-users "https://api.csdn.net/bbs/getreplyusers" "获取回复的用户"
                ((id . "帖子id"))
                ((page . "当前页码，默认1")
                 (size . "每页条数，默认15")))

(provide 'csdn)

;;; csdn.el ends here

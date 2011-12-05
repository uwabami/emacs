
(eval-when-compile (require 'lookup))
(autoload 'lookup "lookup" nil t)
(autoload 'lookup-region "lookup" nil t)
(autoload 'lookup-pattern "lookup" nil t)

(global-set-key "\C-cw" 'lookup-pattern)
(global-set-key "\C-cW" 'lookup-word)

(setq lookup-search-agents
     '(
       (ndeb "/usr/local/share/dict/eijiro" :alias "英辞郎")
       (ndeb "/usr/local/share/dict/waeijiro" :alias "和英辞郎")
       (ndeb "/usr/local/share/dict/rikagaku5" :alias "理化学辞典")
       (ndeb "/usr/local/share/dict/koujien4" :alias "広辞苑")
       (ndeb "/usr/local/share/dict/wadai5" :alias "研究社 和英大辞典")
       ;; (ndeb "/usr/local/share/dict/shizenenja" :alias "自然科学系(英和)")
       ;; (ndeb "/usr/local/share/dict/shizenjaen" :alias "自然科学系(和英)")
       ;; (ndeb "/usr/local/share/dict/colloc" :alias "研究社 英和活用大辞典")
       ;; (ndeb "/usr/local/share/dict/eidai6" :alias "研究社 英和大辞典")
       ;; (ndest "~/Mail/.caskets" :alias "メール全文検索")
       ))

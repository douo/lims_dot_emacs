;; -*- lexical-binding: t; -*-
(defun douo/auth-source-secret (host user)
  "从 auth-source 读取 HOST 和 USER 对应的密钥。"
  (require 'auth-source)
  (when-let* ((entry (car (auth-source-search
                           :host host
                           :user user
                           :require '(:secret)
                           :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defvar llm-gpt4o-provider nil)
(defvar llm-gemini-flash-provider nil)
(defvar llm-gemini-provider nil)
(defvar llm-copilot-provider nil)
(defvar llm-grok2-provider nil)
(defvar llm-grok3-provider nil)
(defvar llm-vertex-gemini-provider nil)

;; 注意：下面的提供器都采用首次使用时创建并缓存的方式。
;; 这样启动时不会加载大语言模型后端，也不会解密 ~/.authinfo.gpg。
(defun douo/llm-gpt4o-provider ()
  "返回缓存的 OpenAI provider。"
  (or llm-gpt4o-provider
      (progn
        (require 'llm-openai)
        (setq llm-gpt4o-provider
              (make-llm-openai
               :key (douo/auth-source-secret "api.openai.com" "apikey")
               :chat-model "gpt-4o-mini")))))

(defun douo/llm-gemini-flash-provider ()
  "返回缓存的 Gemini Flash provider。"
  (or llm-gemini-flash-provider
      (progn
        (require 'llm-gemini)
        (setq llm-gemini-flash-provider
              (make-llm-gemini
               :key (douo/auth-source-secret "generativelanguage.googleapis.com" "apikey")
               :chat-model "gemini-2.5-flash")))))

(defun douo/llm-gemini-provider ()
  "返回缓存的 Gemini provider。"
  (or llm-gemini-provider
      (progn
        (require 'llm-gemini)
        (setq llm-gemini-provider
              (make-llm-gemini
               :key (douo/auth-source-secret "generativelanguage.googleapis.com" "apikey")
               :chat-model "gemini-2.5-pro")))))

(defun douo/llm-copilot-provider ()
  "返回缓存的本地 Copilot 兼容 provider。"
  (or llm-copilot-provider
      (progn
        (require 'llm-openai)
        (setq llm-copilot-provider
              (make-llm-openai-compatible
               :key (douo/auth-source-secret "copilot.p44" "apikey")
               :chat-model "gpt-3.5-turbo"
               :url "http://p44.zero:8080/v1/")))))

(defun douo/llm-grok2-provider ()
  "返回缓存的 Grok 2 provider。"
  (or llm-grok2-provider
      (progn
        (require 'llm-openai)
        (setq llm-grok2-provider
              (make-llm-openai-compatible
               :key (douo/auth-source-secret "api.x.ai" "apikey")
               :chat-model "grok-2-latest"
               :url "https://api.x.ai/v1/")))))

(defun douo/llm-grok3-provider ()
  "返回缓存的 Grok 3 provider。"
  (or llm-grok3-provider
      (progn
        (require 'llm-openai)
        (setq llm-grok3-provider
              (make-llm-openai-compatible
               :key (douo/auth-source-secret "api.x.ai" "apikey")
               :chat-model "grok-3-beta"
               :url "https://api.x.ai/v1/")))))

(defun douo/llm-vertex-gemini-provider ()
  "返回缓存的 Vertex Gemini provider。"
  (or llm-vertex-gemini-provider
      (progn
        (require 'llm-vertex)
        (setq llm-vertex-gemini-provider
              (make-llm-vertex
               :project (douo/auth-source-secret "vertex.ai" "projectid")
               :chat-model "gemini-2.5-pro"
               :embedding-model "gemini-embedding-001")))))

(defun douo/llm-ollama-provider (model)
  "为 MODEL 创建 Ollama provider。"
  (require 'llm-ollama)
  (make-llm-ollama
   :host "p44.zero"
   :chat-model model
   :embedding-model model))

(defun douo/setup-aidermacs-api-keys ()
  "按需设置 Aidermacs 使用的 API key 环境变量。"
  (setenv "XAI_API_KEY" (douo/auth-source-secret "api.x.ai" "apikey"))
  (setenv "GEMINI_API_KEY"
          (douo/auth-source-secret "generativelanguage.googleapis.com" "apikey")))

(use-package gptel
  :straight t
  :disabled)

(use-package llm
  :straight t
  ;; 注意：基础 llm 包也懒加载，具体后端由上面的提供器函数按需加载。
  :defer t
  :custom
  (llm-warn-on-nonfree nil))

;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :config
;;   (setq aider-args '("--model" "gpt-4o-mini"))
;;   (setenv "OPENAI_API_KEY" (auth-info-password
;;                             (car (auth-source-search
;;                                   :host "api.openai.com"
;;                                   :user "apikey"))))
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :bind (("C-c a" . aidermacs-transient-menu))
  :init
  ;; 注意：打开 Aidermacs 菜单前才读取密钥，避免启动时触发 auth-source 或 GPG。
  (advice-add 'aidermacs-transient-menu :before #'douo/setup-aidermacs-api-keys)
  :custom
  ;; 详见 aidermacs 的配置文档。
  (aidermacs-use-architect-mode t)
  (aidermacs-backend 'vterm)
  (aidermacs-default-model "gemini"))



(use-package magit-gptcommit
  :straight t
  ;; 注意：等 `magit-commit' transient 定义后再挂接，否则 GPT Commit 菜单不会出现。
  :after magit-commit
  :demand t
  :custom
  (magit-gptcommit-llm-provider #'douo/llm-gemini-flash-provider)
  :config
  ;; 启用 magit-gptcommit-mode 后，可在 Magit 状态缓冲区监听暂存区并自动生成提交信息。
  ;; 这个模式是可选的，也可以手动调用 `magit-gptcommit-generate' 生成提交信息。
  ;; `magit-gptcommit-generate' 当前只应在 magit status buffer 中执行。
  ;; (magit-gptcommit-mode 1)

  ;; 向 `magit-commit' 添加 gptcommit 菜单命令。
  ;; 如需移除，可执行 (transient-remove-suffix 'magit-commit '(1 -1))。
  (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))


;; llm
(use-package ellama  ;; 依赖 https://github.com/ahyatt/llm
  :straight t
  ;; 注意：通过 s-o 前缀首次触发时才加载 ellama；提供器也会在加载后再创建。
  :bind-keymap ("s-o" . ellama-command-map)
  :config
  ;; 键位已经由 :bind-keymap 注册，这里避免 ellama 再设置前缀。
  (setopt ellama-keymap-prefix nil)
  ;; ellama 翻译目标语言。
  (setopt ellama-language "Chinese")
  (setopt ellama-provider
          (douo/llm-ollama-provider "llama3"))
  ;; 预定义提供器用于交互式切换。
  ;; 注意：这些提供器会在 ellama 首次加载时创建，而不是 Emacs 启动时创建。
  (setopt ellama-providers
          `(("llama3" . ,(douo/llm-ollama-provider "llama3"))
            ("dolphin-llama3" . ,(douo/llm-ollama-provider "dolphin-llama3:8b-v2.9-fp16"))
            ("qwen" . ,(douo/llm-ollama-provider "qwen:32b"))
            ("gpt4o" . ,(douo/llm-gpt4o-provider))
            ("gemini-2.0-flash" . ,(douo/llm-gemini-provider))
            ("copilot" . ,(douo/llm-copilot-provider))
            ))
  ;; 使用 llm 为新会话命名。
  (setopt ellama-naming-provider ellama-provider)
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; 翻译使用的 llm 提供器。
  (setopt ellama-translation-provider ellama-provider)
  (defcustom douo/ellama-code-explain-prompt-template "You are a code explanation engine that can only explain code but not interpret or translate it. Also, please report bugs and errors (if any).\nexplain the provided code,regex or script in the most concise language and must use %s language!You may use Markdown.If the content is not code,return an error message.If the code has obvious errors, point them out.```\n%s\n```"
    "Prompt template for `ellama-code-explain'."
    :group 'ellama
    :type 'string)
  (defcustom douo/ellama-code-explain-region-prompt-template "You are a code explanation engine that can only explain code but not interpret or translate it. Also, please report bugs and errors (if any).\nexplain the provided code,regex or script in the most concise language and must use %s language!You may use Markdown.If the content is not code,return an error message.If the code has obvious errors, point them out. First provided code block is the context of target code. second provided code block is code you need to explain\n```\n%s\n```\n```\n%s\n```"
    "Prompt template for `ellama-code-explain'."
    :group 'ellama
    :type 'string)
  (defun douo/ellama-code-explain ()
    "Explain the code at point using ellama."
    (interactive)
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          )
      (ellama-instant
       (if (region-active-p)
           (format douo/ellama-code-explain-region-prompt-template
                   ellama-language
                   content
                   (buffer-substring-no-properties (region-beginning) (region-end)))
         (format douo/ellama-code-explain-prompt-template
                 ellama-language
                 content)))))
  :bind
  (:map ellama-command-map
        ("c d" . douo/ellama-code-explain)))


(use-package immersive-translate
  :straight t
  ;; 注意：只注册交互命令，首次调用翻译功能时才加载包和 llm 相关依赖。
  :commands (immersive-translate-buffer
             immersive-translate-paragraph
             immersive-translate-clear
             immersive-translate-auto-mode
             immersive-translate-abort
             immersive-translate-setup)
  :custom
  (immersive-translate-backend 'chatgpt)
  (immersive-translate-chatgpt-host "api.x.ai")
  (immersive-translate-chatgpt-model "grok-2-latest")
  (immersive-translate-failed-message "💢"))

(provide 'init-llm)

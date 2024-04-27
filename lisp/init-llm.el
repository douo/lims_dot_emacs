;; gpt
(use-package gptel
  :straight t)

(use-package magit-gptcommit
  :straight t
  :demand t
  :after gptel magit
  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  )


;; llm
(use-package ellama  ;; 依赖 https://github.com/ahyatt/llm
  :straight t
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "s-o")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
           :host "p44.zero"
	   :chat-model "llama3"
	   :embedding-model "llama3"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
	  '(("llama3" . (make-llm-ollama
                            :host "p44.zero"
		            :chat-model "llama3"
			    :embedding-model "llama3"))
	    ("dolphin-llama3" . (make-llm-ollama
                                 :host "p44.zero"
			         :chat-model "dolphin-llama3:8b-v2.9-fp16"
			         :embedding-model "dolphin-llama3:8b-v2.9-fp16"))
	    ("qwen" . (make-llm-ollama
                       :host "p44.zero"
		       :chat-model "qwen:32b"
		       :embedding-model "qwen:32b"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider ellama-provider)
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider ellama-provider)
  :config
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
  ("s-o c d" . douo/ellama-code-explain)
  )


(provide 'init-llm)

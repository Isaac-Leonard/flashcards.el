;;; flashcards.el --- Test a user on a file of pre-formatted flashcards  -*- lexical-binding: t; -*-

;; Author: Isaac Leonard
;; Version: 1.0

;;; Code:
(defun flashcards ()
  "Begins testing the user with flashcards"
  (interactive)
  (message-if-not-nil (catch 'parse-error
    (let* ((flashcard-file (read-file-name "Pick a file to read flashcards from: "))
	   (flashcards-text (f-read-text flashcard-file))
	   (flashcards-list (flashcards-parse flashcards-text)))
      (switch-to-buffer "*Flashcards*")
      (setq-local flashcards flashcards-list
		  answering nil)
      (local-set-key (kbd "C-c C-c") 'flashcard-next)
      (flashcard-next))
    nil)))

(defun flashcards-parse (flashcards-text)
  "Parses flashcards into a list of objects with question and answer properties"
  (let* ((flashcards-list (list))
	(current-token "")
	(question "")
	(answer "")
	(parsing nil)
	(chars (map 'list 'char-to-string (string-to-list flashcards-text))))
    (cl-loop for char in chars do
	     (setq current-token (concat current-token char))
	     (cond ((string= (string-trim current-token) "Question:")
		    (setq current-token "")
		    (cond
		     ((not parsing) (setq parsing "question"))
		     ((string= parsing "answer") (push (make-flashcard
							:question (string-trim question)
							:answer (string-trim answer))
						       flashcards-list)
		      (setq parsing "question")
		      (setq question "")
		      (setq answer ""))
		     ((string= parsing "question") (throw 'parse-error "found 'Question:' in a question"))))
		   ((string= current-token "Answer:")
		    (setq current-token "")
		    (cond
		     ((string= parsing "question") (setq parsing "answer"))
		     ((not parsing ) (throw 'parse-error "File started with an answer not a question"))
		     ((string= parsing "answer") (throw 'parse-error "Found 'Answer:' in an answer"))))
		   ;; If the current word has trailing whitespace then append it to the item being parsed
		   ((not (= (length (string-trim current-token)) (length current-token)))
		    (if (string= parsing "question")
			(setq question (concat question current-token))
		      (setq answer (concat answer current-token)))
		    (setq current-token ""))))
    (push (make-flashcard
	   :question (string-trim question)
	   :answer (string-trim answer))
	  flashcards-list)
    flashcards-list))

(cl-defstruct (flashcard (:constructor make-flashcard))
  question
  answer)

(defun flashcard-next ()
  "Displays the correct answer if its not already displayed, otherwise it goes to the next question"
  (interactive)
  (if answering
      (progn (jump-to-end-of-buffer)
	     (insert  "\nReal answer:" (flashcard-answer current-flashcard))
	     (setq answering nil)
	     (message (flashcard-answer current-flashcard)))
    (progn
      (next-card)
      (setq answering t))))

(defun next-card ()
  "Displays the next card"
  (setq answering nil)
  (flashcard-random)
  (let* ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ()))
  (delete-region (point-min) (point-max))
  (insert "Question:\n" (flashcard-question current-flashcard) "\nYour answer:\n")
  (put-text-property 1 2 'front-sticky '(read-only))
  (put-text-property (- (point-max) 2) (point-max) 'rear-sticky nil)
  (put-text-property (point-min) (- (point-max) 1) 'read-only t)
  (message (flashcard-question current-flashcard)))

(defun flashcard-random ()
  "Sets a random flashcard to current-flashcard"
  (setq-local current-flashcard (nth (+ (random (length flashcards))) flashcards)))

(defun message-if-not-nil (message)
  "Displays the argument if it is not nil"
  (if (not (not message)) (message message)))

(provide 'flashcards)
;;; flashcards.el ends here

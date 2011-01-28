;;; my-circe.el --- circe

;; Copyright (C) 2007  Michael Trommer

;; Author: Michael Trommer <mictro@gmail.com>

(defun circ ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode")
  ;;(circe "irc.quakenet.org" "6667" "quakenet")
  ;;(circe "tie" "6667" "bitlbee")      
  (circe "irc.tu-ilmenau.de" "6668" "IRCnet")
  )

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (enable-lui-irc-colors)
     (add-to-list 'circe-receive-message-functions
                  'mt-bitlbee-auth)))


(autoload 'circe "circe" "Connect to an IRC server" t)
(when (file-directory-p "~/.emacs.d/circe")
  (add-to-list 'load-path "~/.emacs.d/circe"))


(setq circe-default-realname "mtrommer"
      circe-default-nick "entropie"
      circe-server-coding-system '(utf-8 . undecided)
      circe-server-killed-confirmation 'ask-and-kill-all
      circe-server-auto-join-channels
      '(("^freenode$" "#emacs" "#ruby-de" "#ruby-lang" "#ackro" "#emacs" )
        ;;("^quakenet$" "#ackro")
        ;;("^ircnet" "+linux.de")
        )
      circe-nickserv-passwords `(("freenode", freenode-passwd))
      circe-format-server-topic "*** topic change by {origin}: {topic-diff}")

(setq lui-max-buffer-size 23000
      lui-fill-column 91                ; 66
      lui-flyspell-p t
      lui-flyspell-alist '(("#ackro" "german8")
                           ("+linux.de" "german8")
                           ("#ruby-de" "german8")
                           ("#wikipedia*" "german8")
                           ("." "american"))

      lui-highlight-keywords `((,(let ((nicks '(or "rretzbach"
                                                   "shafire"
                                                   )))
                                   (if (< emacs-major-version 22)
                                       nil
                                     (rx (or (: bos
                                                "<" (* (not (any ">")))
                                                (eval nicks)
                                                (* (not (any ">")))
                                                ">"
                                                (* anything))
                                             (: bos
                                                "* " (* (not (any whitespace)))              
                                                (eval nicks)
                                                (* anything))
                                             (: bos
                                                "<" (* (not (any ">"))) "> "
                                                (* (not (any whitespace)))
                                                (eval nicks)
                                                (* (not (any whitespace)))
                                                (or ":" " - " ",")
                                                (* anything)
                                                )))))
                                (lui-fool t
                                          face mt-lui-fool-face))))

(defun mt-bitlbee-auth (nick user host command args)
  "Authenticate to a bitlbee server."
  (when (and (string= command "JOIN")
             (circe-server-my-nick-p nick))
    (with-circe-server-buffer
     (when (string= circe-server-network "bitlbee")
       (circe-server-send
        (format "PRIVMSG #bitlbee :identify %s"
                bitlbee-passwd))))))


(defface mt-lui-fool-face
  '((t (:foreground "seashell4")))
  "A face for fools on IRC. Readable, but not disturbing.")

(defadvice circe-server-send-queue (after fc-flood-debug (buffer) activate)
  "Debug IRC throttling."
  (with-current-buffer buffer
    (when (not (null circe-server-flood-queue))
      (let ((len (length circe-server-flood-queue))
            (q circe-server-flood-queue))
        (circe-server-message (format "Throttling (%s message%s)" len
                                      (if (= len 1) "" "s")))))))
(ad-activate 'circe-server-send-queue)

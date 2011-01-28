<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html><head><title>EmacsWiki: legalese.el</title><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,NOFOLLOW" /><link rel="alternate" type="application/rss+xml" title="Emacs Wiki with page content" href="http://www.emacswiki.org/cgi-bin/wiki?action=rss;full=1" /><link rel="alternate" type="application/rss+xml" title="Emacs Wiki with page content and diff" href="http://www.emacswiki.org/cgi-bin/wiki?action=rss;full=1;diff=1" /><link rel="alternate" type="application/rss+xml" title="Emacs Wiki including minor differences" href="http://www.emacswiki.org/cgi-bin/wiki?action=rss;showedit=1" /></head><body class="http://www.emacswiki.org/cgi-bin/emacs-en"><div class="header"><a class="logo" href="http://www.emacswiki.org/cgi-bin/emacs-en/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Search">Search</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/News">News</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Suggestions">Suggestions</a> </span><form class="tiny" action="http://www.emacswiki.org/cgi-bin/emacs-en"><p>Search: <input type="text" name="search" size="20" /></p></form><h1><a title="Click to search for references to this page" href="http://www.emacswiki.org/cgi-bin/emacs-en?search=%22legalese%5c.el%22">legalese.el</a></h1></div><div class="content browse"><p><p><a href="http://www.emacswiki.org/cgi-bin/wiki/download/legalese.el">Download</a></p><pre class="source"><pre class="code"><span class="linecomment">;;; legalese.el --- Add legalese to your program files</span>

<span class="linecomment">;; Copyright (C) 2004  Jorgen Schaefer &lt;forcer@forcix.cx&gt;</span>

<span class="linecomment">;; Author: Jorgen Schaefer &lt;forcer@forcix.cx&gt;</span>
<span class="linecomment">;; Keywords: convenience</span>

<span class="linecomment">;; This file is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 2, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; This file is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with GNU Emacs; see the file COPYING.  If not, write to</span>
<span class="linecomment">;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,</span>
<span class="linecomment">;; Boston, MA 02111-1307, USA.</span>

<span class="linecomment">;;; Commentary:</span>

<span class="linecomment">;; This file adds the sometimes necessary legalese to your files. That</span>
<span class="linecomment">;; is, it adds a license header. For most files, it tries to adhere to</span>
<span class="linecomment">;; a nice standard layout.</span>

<span class="linecomment">;; I recommend the following setup:</span>

<span class="linecomment">;; (setq comment-style 'extra-line)</span>
<span class="linecomment">;; (add-hook 'scheme-mode-hook</span>
<span class="linecomment">;;           (lambda ()</span>
<span class="linecomment">;;             (set (make-local-variable 'comment-add) 1)))</span>

<span class="linecomment">;;; Code:</span>

(defvar legalese-version "<span class="quote">1.0</span>"
  "<span class="quote">Version string of legalese.el</span>")

(defgroup legalese nil
  "<span class="quote">*Add legalese to your files.</span>"
  :prefix "<span class="quote">legalese-</span>"
  :group 'programming)

(defcustom legalese-default-copyright nil
  "<span class="quote">*The string to use by default for the copyright holder. If this is
nil, the current users' mail address is used.</span>"
  :group 'legalese
  :type '(choice string
                 (const :tag "<span class="quote">Default address</span>" nil)))

(defcustom legalese-default-author nil
  "<span class="quote">*The string to use by default for the author. If this is nil, the
current users' mail address is used.</span>"
  :group 'legalese
  :type '(choice string
                 (const :tag "<span class="quote">Default address</span>" nil)))

(defcustom legalese-default-license 'gpl
  "<span class="quote">*The default license to use.</span>"
  :group 'legalese
  :type '(choice (const :tag "<span class="quote">General Public License</span>" gpl)
                 (const :tag "<span class="quote">Lesser General Public License</span>" lgpl)
                 (const :tag "<span class="quote">Free Documentation License</span>" fdl)))

(defcustom legalese-templates
  '((emacs-lisp-mode (nil "<span class="quote">;;; </span>" legalese-file-name "<span class="quote"> --- </span>" _ "<span class="quote">\n</span>"
                          "<span class="quote">\n</span>"
                          "<span class="quote">;; Copyright (C) </span>" legalese-year "<span class="quote">  </span>" legalese-copyright "<span class="quote">\n</span>"
                          "<span class="quote">\n</span>"
                          "<span class="quote">;; Author: </span>" legalese-author "<span class="quote">\n</span>"
                          <span class="linecomment">;; This looks weird. Taken from autoinsert.</span>
                          <span class="linecomment">;; *shrug* :-)</span>
                          "<span class="quote">;; Keywords: </span>" ((legalese-elisp-keyword)
                                           str "<span class="quote">, </span>") & -2 "<span class="quote">\n</span>"
                          "<span class="quote">\n</span>"
                          @
                          '(legalese-license)
                          @
                          "<span class="quote">\n</span>"
                          "<span class="quote">;;; Commentary:\n</span>"
                          "<span class="quote">\n</span>"
                          "<span class="quote">;;; Code:\n</span>"
                          "<span class="quote">\n</span>"
                          "<span class="quote">\n</span>"
                          "<span class="quote">(provide '</span>" legalese-file "<span class="quote">)\n</span>"
                          "<span class="quote">;;; </span>" legalese-file-name "<span class="quote"> ends here\n</span>"))
    (scheme-mode (nil "<span class="quote">;;; </span>" legalese-file-name "<span class="quote"> --- </span>" _ "<span class="quote">\n</span>"
                      "<span class="quote">\n</span>"
                      "<span class="quote">;; Copyright (C) </span>" legalese-year "<span class="quote"> </span>" legalese-copyright "<span class="quote">\n</span>"
                      "<span class="quote">\n</span>"
                      "<span class="quote">;; Author: </span>" legalese-author "<span class="quote">\n</span>"
                      "<span class="quote">\n</span>"
                      @
                      '(legalese-license)
                      @
                      "<span class="quote">\n</span>"
                      "<span class="quote">;;; Commentary:\n</span>"
                      "<span class="quote">\n</span>"
                      "<span class="quote">;;; Code:\n</span>"
                      "<span class="quote">\n</span>"))
    (default (nil @
                  legalese-file-name "<span class="quote"> --- </span>" _ "<span class="quote">\n</span>"
                  "<span class="quote">\n</span>"
                  "<span class="quote">Copyright (C) </span>" legalese-year "<span class="quote"> </span>" legalese-copyright "<span class="quote">\n</span>"
                  "<span class="quote">\n</span>"
                  "<span class="quote">Author: </span>" legalese-author "<span class="quote">\n</span>"
                  "<span class="quote">\n</span>"
                  '(legalese-license)
                  @)))
  "<span class="quote">*A list that associates a major mode with the appropriate skeleton
to use for this mode. The region between the last two @ marks will be
commented out using comment-region.</span>"
  :group 'legalese
  :type '(alist :key-type symbol
                :value-type (repeat sexp)))

(defcustom legalese-licenses
  '((gpl "<span class="quote">This program is free software; you can redistribute it and/or</span>"
         "<span class="quote">modify it under the terms of the GNU General Public License</span>"
         "<span class="quote">as published by the Free Software Foundation; either version 2</span>"
         "<span class="quote">of the License, or (at your option) any later version.</span>"
         "<span class="quote"></span>"
         "<span class="quote">This program is distributed in the hope that it will be useful,</span>"
         "<span class="quote">but WITHOUT ANY WARRANTY; without even the implied warranty of</span>"
         "<span class="quote">MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>"
         "<span class="quote">GNU General Public License for more details.</span>"
         "<span class="quote"></span>"
         "<span class="quote">You should have received a copy of the GNU General Public License</span>"
         "<span class="quote">along with this program; if not, write to the Free Software</span>"
         "<span class="quote">Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA</span>"
         "<span class="quote">02111-1307, USA.</span>")
    (lgpl "<span class="quote">This library is free software; you can redistribute it and/or</span>"
          "<span class="quote">modify it under the terms of the GNU Lesser General Public</span>"
          "<span class="quote">License as published by the Free Software Foundation; either</span>"
          "<span class="quote">version 2.1 of the License, or (at your option) any later version.</span>"
          "<span class="quote"></span>"
          "<span class="quote">This library is distributed in the hope that it will be useful,</span>"
          "<span class="quote">but WITHOUT ANY WARRANTY; without even the implied warranty of</span>"
          "<span class="quote">MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU</span>"
          "<span class="quote">Lesser General Public License for more details.</span>"
          "<span class="quote"></span>"
          "<span class="quote">You should have received a copy of the GNU Lesser General Public</span>"
          "<span class="quote">License along with this library; if not, write to the Free Software</span>"
          "<span class="quote">Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA</span>"
          "<span class="quote">02111-1307  USA</span>")
    (fdl "<span class="quote">Permission is granted to copy, distribute and/or modify this document</span>"
         "<span class="quote">under the terms of the GNU Free Documentation License, Version 1.1</span>"
         "<span class="quote">or any later version published by the Free Software Foundation;</span>"
         "<span class="quote">with the Invariant Sections being LIST THEIR TITLES, with the</span>"
         "<span class="quote">Front-Cover Texts being LIST, and with the Back-Cover Texts being LIST.</span>"
         "<span class="quote">A copy of the license is included in the section entitled \</span>"GNU"<span class="quote">
         </span>"Free Documentation License\"<span class="quote">.</span>"))
  "<span class="quote">*A definition of copyright boilerplates.</span>"
  :group 'legalese
  :type '(alist :key-type symbol
                :value-type (repeat string)))

(defun legalese (ask)
  "<span class="quote">Add standard legalese prelude to the current buffer. With
prefix-argument ASK, ask for a license to use.</span>"
  (interactive "<span class="quote">P</span>")
  (let ((legalese-year (format-time-string "<span class="quote">%Y</span>"))
        (legalese-copyright (or legalese-default-copyright 
                                (concat user-full-name
                                        "<span class="quote"> &lt;</span>" user-mail-address "<span class="quote">&gt;</span>")))
        (legalese-author (or legalese-default-author
                             (concat user-full-name
                                     "<span class="quote"> &lt;</span>" user-mail-address "<span class="quote">&gt;</span>")))
        (legalese-file-name (file-name-nondirectory (buffer-file-name)))
        (legalese-file (file-name-nondirectory
                        (file-name-sans-extension
                         (buffer-file-name))))
        (legalese-license (if (car ask)
                              (intern
                               (completing-read "<span class="quote">License: </span>"
                                                (mapcar
                                                 (lambda (item)
                                                   (list
                                                    (symbol-name
                                                     (car item))))
                                                 legalese-licenses)
                                                nil
                                                t))
                            legalese-default-license)))
    (skeleton-insert (cadr (or (assq major-mode legalese-templates)
                               (assq 'default legalese-templates))))
    (let ((beg (cadr skeleton-positions))
          (end (car skeleton-positions)))
      (comment-region beg end)
      (setq skeleton-positions (cddr skeleton-positions)))))

(defun legalese-license ()
  "<span class="quote">Add the license of `legalese-default-license' here.</span>"
  (insert (mapconcat #'identity
                     (cdr (assq legalese-license
                                legalese-licenses))
                     "<span class="quote">\n</span>"))
  (insert "<span class="quote">\n</span>"))

(defun legalese-elisp-keyword ()
  "<span class="quote">Add emacs lisp keywords.</span>"
  (require 'finder)
  (let ((keywords (mapcar (lambda (x) (list (symbol-name (car x))))
                          finder-known-keywords))
	(minibuffer-help-form (mapconcat (lambda (x) (format "<span class="quote">%10.0s:  %s</span>"
                                                             (car x)
                                                             (cdr x)))
                                         finder-known-keywords
                                         "<span class="quote">\n</span>")))
    (completing-read "<span class="quote">Keyword, C-h: </span>" keywords nil t)))


(provide 'legalese)
<span class="linecomment">;;; legalese.el ends here</span></span></pre></pre></p></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Search">Search</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/News">News</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs-en/Suggestions">Suggestions</a> </span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" href="http://www.emacswiki.org/cgi-bin/emacs-en?action=edit;id=legalese.el">Edit this page</a> <a class="history" href="http://www.emacswiki.org/cgi-bin/emacs-en?action=history;id=legalese.el">View other revisions</a> <a class="admin" href="http://www.emacswiki.org/cgi-bin/emacs-en?action=admin;id=legalese.el">Administration</a></span><span class="time"><br /> Last edited 2005-10-13 17:56 UTC by <a class="author" title="from 217-162-112-104.dclient.hispeed.ch" href="http://www.emacswiki.org/cgi-bin/emacs-en/AlexSchroeder">AlexSchroeder</a> <a class="diff" href="http://www.emacswiki.org/cgi-bin/emacs-en?action=browse;diff=2;id=legalese.el">(diff)</a></span><form method="get" action="http://www.emacswiki.org/cgi-bin/emacs-en" enctype="multipart/form-data" class="search">
<p><label for="search">Search:</label> <input type="text" name="search"  size="20" accesskey="f" id="search" /> <label for="searchlang">Language:</label> <input type="text" name="lang"  size="10" id="searchlang" /> <input type="submit" name="dosearch" value="Go!" /></p><div></div>
</form><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="http://creativecommons.org/images/public/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p>
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div></body></html>
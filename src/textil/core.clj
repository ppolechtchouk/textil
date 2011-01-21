(ns textil.core)

(defn trim-whitespace
  "Returns a string with excessive whitespace removed"
  [#^String s]
  (str-join "\n" 
	    (map #(.trim %) (re-split (re-pattern "\n") s))))

(defn html-paragraphs
  "Returns a string with paragraphs surrounded by <p></p> tags. trim-whitespace should be applied first.
End of paragraph is considered to be a double newline, i.e. \\n\\n"
  [#^String s]
  (apply str
	 (map #(format "<p>%s</p>\n" %)
	       (re-split #"\n\n" s))))

(defn html-highlight-quote
  "Returns a string with all text within ' ' characters enclosed in the <span =\"highlight\">"
  [#^String s]
  (.replaceAll s "(?<=^|[\\s(])'(.*?)'(?=$|[\\s\\.,:;)-])" "<span class=\"highlight\">$1</span>"))

(defn html-highlight-dquote
  "Returns a string with all text within \" \" characters enclosed in the <span =\"highlight\">. 
Unlike the 'html-highlight-quote', the double quotes will still be present.
Note that this will cause problems with html tags! So it should be used before any tags are present."
  [#^String s]
  (.replaceAll s "(?<=^|[\\s(])(\".*?\")(?=$|[\\s\\.,:;)-])" "<span class=\"highlight\">$1</span>"))

(defn curly-bracers-to-element-links
  "Convert the element name surrounded by curly bracers to an XML element link.
i.e. {tml} -> <tml> & link to the tml.html page."
  [#^String s]
  (.replaceAll s "(?<=^|[\\s\\n(])\\{([\\w]+)\\}(?=$|[\\s\\n.,:;)])" "<a class=\"element\" href=\"$1.html\">&lt;$1&gt;</a>"))

(defn star-to-li2
  "Returns a string with lines starting with a ** character (wiki-style list syntax) replaced by the <li class=\"list2\">text</li>"
  [#^String s]
  (.replaceAll s "(?s)(^|\n)\\s*\\*\\*\\s+(((?!\n\n|\n\\s*\\*+\\s|<ul|</ul|<li|</li).)*)" "$1<li class=\"list2\">$2</li>")); using lookahead and lookbehind regex

(defn star-to-li1
  "Returns a string with lines starting with a * character (wiki-style list syntax) replaced by the <li class=\"list1\">text</li>"
  [#^String s]
  (.replaceAll s "(?s)(^|\n)\\s*\\*\\s+(((?!\n\n|\n\\s*\\*+\\s|<ul|</ul|<li|</li).)*)" "$1<li class=\"list1\">$2</li>")); using lookahead and lookbehind regex

(defn wrap-ul
  "Returns a string with list items (defined by *) that are next to each other wrapped by <ul>. It may be usefult to trim-whitespace and sanitize-newlines first"
  [#^String s]
  (.replaceAll s "(?s)(^|\n)(\\s*\\*+\\s((?!\n\n).)*)" "$1<ul>\n$2</ul>\n"))

(defn html-ul
  "Returns a string with all * items wrapped as html unordered lists"
  [#^String s]
  (star-to-li2 (star-to-li1 (wrap-ul s))))

(defn hash-to-numbered-li1
  "Returns a string with lines starting with a * character (wiki-style list syntax) replaced by the <li class=\"list1\">text</li>"
  [#^String s]
  (.replaceAll s "(?s)(^|\n)\\s*#\\s+(((?!\n\n|\n\\s*#+\\s|<ol|</ol|<li|</li).)*)" "$1<li class=\"num_list1\">$2</li>")); using lookahead and lookbehind regex

(defn wrap-ol
  "Returns a string with numbered list items (defined by #) that are next to each other wrapped by <ol>. It may be usefult to trim-whitespace and sanitize-newlines first"
  [#^String s]
  (.replaceAll s "(?s)(^|\n)(\\s*#+\\s((?!\n\n).)*)" "$1<ol>\n$2</ol>\n"))

(defn html-ol
  "Returns a string with all * items wrapped as html unordered lists"
  [#^String s]
  (hash-to-numbered-li1 (wrap-ol s)))

(defn sanitize-newlines1
  "Replaces the newlines in the middle of a sentence by a space. s should have trim-whitespace applied first."
  [#^String s]
  (.replaceAll s "(?m)(?<!(\n|:|\\.|;|,))\n(?!(^\\s*$))" " "))

(defn sanitize-newlines
  [#^String s]
  (str-join "\n* "
	 (map sanitize-newlines1 (re-split (re-pattern "(\n|^)(\\s*?)\\*\\s") s))))

(defn html-br
  [#^String s]
  (str-join ".<br/>\n"
	    (re-split (re-pattern "\\.\n") s)))

; a list of text procesing functions. They are done in order of first to last 
(def *process-text-fns* (list default-message trim-whitespace ;sanitize-newlines  
			      html-highlight-dquote ; no html tags should be present before this is called.		    
			      html-highlight-quote
			      curly-bracers-to-element-links
                              html-ol
			      html-ul html-paragraphs html-br))

(defn process-text1
  [s fn-list]
  (if fn-list
    (process-text1 ((first fn-list) s) (next fn-list))
    s))

(defn process-text
  "Returns a string with the text processing functions applied"
  [#^String s]
  (process-text1 s *process-text-fns*))


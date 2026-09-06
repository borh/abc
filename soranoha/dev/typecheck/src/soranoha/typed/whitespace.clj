(ns soranoha.typed.whitespace
  (:require [typed.clojure :as t]))
(t/defalias SourceText (t/Option t/Str))
(t/defalias Inline (t/U t/Str '[':lb]))
(t/ann soranoha.ori.publication-whitespace/normalize-newlines [SourceText :-> t/Str])
(t/ann soranoha.ori.publication-whitespace/source-text->plaintext [SourceText t/Bool :-> t/Str])
(t/ann soranoha.ori.publication-whitespace/source-text-ends-with-newline? [SourceText :-> t/Bool])
(t/ann soranoha.ori.publication-whitespace/trim-trailing-newlines [SourceText :-> t/Str])
(t/ann soranoha.ori.publication-whitespace/source-text->tei-inline [SourceText :-> (t/Seq Inline)])

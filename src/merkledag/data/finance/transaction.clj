(ns merkledag.data.finance.transaction
  "Functions dealing with financial transactions."
  (:require
    [datascript.core :as d]
    (merkledag.data.finance
      [entry :as entry]
      [types :as types])))


; Static checks:
; - must be at least one entry
; - all entry accounts belong to the same books
; - real posting weights must sum to zero

; Historical checks:
; ...

(ns merkledag.data.finance.posting
  "Functions dealing with postings to financial accounts."
  (:require
    [datascript.core :as d]))


; Static checks:
; - schema validates
; - amount and price must have different commodities
; - weight only makes sense when price is specified
; - weight must be in same commodity as price
; - weight must be within tolerance of amount * price
; - balance check should match commodity in amount
; - total of items in invoice must match amount
; - commodity in amount must be allowed by account

; Historical checks:
; - lot-id should specify a real previous posting
; - lot-cost and lot-date should match identified posting
; - balance amount should match current account value for that commodity
;   (within tolerance)

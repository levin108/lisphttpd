(in-package :lisphttpd)

(deftype ub8  () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))

(deftype ub8-vector (&optional (size '*))
  `(vector ub8 ,size))

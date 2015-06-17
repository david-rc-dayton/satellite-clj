(ns satellite-clj.time
  "Helper functions for dealing with time.")

;;;; Default Values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pattern-default (atom "yy-DDD HH:mm:ss"))

(defn pattern-default!
  [s]
  (reset! pattern-default s))

;;;; Date Parsing/Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn now
  []
  (java.util.Date.))

(defn date-parse
  ([s]
    (date-parse s @pattern-default))
  ([s pattern]
    (let [sd (doto (java.text.SimpleDateFormat. pattern)
               (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (.parse sd s))))

(defn date-format
  ([date]
    (date-format date @pattern-default))
  ([date pattern]
    (let [sd (doto (java.text.SimpleDateFormat. pattern)
               (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (.format sd date))))

;;;; Epoch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn millis->days
  [millis]
  (/ millis 1000 60 60 24))

(def j2000 (date-parse "00-001 11:58:55" "yy-DDD HH:mm:ss"))

(defn j2000-days
  [date]
  (millis->days (- (.getTime ^java.util.Date date)
                   (.getTime ^java.util.Date j2000))))

(defn gmst
  [date]
  (let [d (j2000-days date)
        h (mod (+ 18.697374558 (* 24.06570982441908 d)) 24)]
    (* 2 Math/PI (/ h 24))))

(defn day-of-year
  "Calculate the number of days elapsed since the beginning of the year. Takes
   a `java.util.Date` object for the starting time as its only argument.

   Returns a floating point representation of elapsed time in the UTC timezone."
  [date]
  (let [cal (doto (java.util.Calendar/getInstance
                    (java.util.TimeZone/getTimeZone "UTC"))
              (.setTime date))
        d (.get cal java.util.Calendar/DAY_OF_YEAR)
        h (.get cal java.util.Calendar/HOUR_OF_DAY)
        m (.get cal java.util.Calendar/MINUTE)
        s (.get cal java.util.Calendar/SECOND)
        f (/ (+ (* h 3600) (* m 60) s) 86400)]
    (double (+ d f))))

(ns datetime-data.core
  (:require [clj-time.coerce :as tc]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clojure.string :as string]))

(defn get-tz
  "Given a timezone id string or offset number return a timezone."
  ([]
   (get-tz "UTC"))
  ([tz]
   (if (string? tz)
     (t/time-zone-for-id tz)
     (t/time-zone-for-offset tz))))

;; we don't really want time zone. The time is what it is wherever it is.
;; we can always bring it back with (t/default-time-zone)
(defn get-yyyy-date-parser
  ([]
   (get-yyyy-date-parser "UTC"))
  ([tz]
   (tf/formatter (get-tz tz)
                 "YYYY"
                 "YYYYMMdd"
                 "YYYY-MM-dd"
                 "YYYY/MM/dd"
                 "MM-dd-YYYY"
                 "MM/dd/YYYY"

                 "YYYY-MM-dd HH:mm:ss"
                 "YYYY/MM/dd HH:mm:ss"
                 "MM-dd-YYYY HH:mm:ss"
                 "MM/dd/YYYY HH:mm:ss"
                 "YYYY-MM-dd HH:mm:ss.SSS"
                 "YYYY/MM/dd HH:mm:ss.SSS"
                 "MM-dd-YYYY HH:mm:ss.SSS"
                 "MM/dd/YYYY HH:mm:ss.SSS")))



;; Show all the available built-in formatters
;; (f/show-formatters)
(defn show-formatters []
  (tf/show-formatters))

(defn date->iso
  "Convert epoch long or date string using yyyy-date-parser to an iso date"
  ([date]
   (date->iso date "UTC"))
  ([date tz]
   (if (number? date)
     (tc/from-long (long date))
     (tf/parse (get-yyyy-date-parser tz) date))))

;; doesn't work. CachedDateTimeZone cannot be cast to java.lang.String.
;; (defn date-format->iso
;;   "convert a date of the given format to an iso date."
;;   [date format]
;;   (let [date-parser (tf/formatter (t/default-time-zone) format)]
;;     (tf/parse date-parser date)))

(defn str-iso-date->iso-date
  "Convert string version of iso date back into iso date."

  ([str-date]
   (str-iso-date->iso-date str-date "UTC"))
  ([str-date tz]
   (tf/parse (tf/formatter (tf/formatters :date-time) (get-tz tz)) str-date)))

(defn str-date->epoch
  ([str-date]
   (str-date->epoch str-date "UTC"))
  ([str-date tz]
   (tc/to-long (date->iso str-date tz))))

(defn str-iso->epoch
  ([str-iso]
   (str-iso->epoch str-iso "UTC"))
  ([str-iso tz]
   (tc/to-long (str-iso-date->iso-date str-iso tz))))

(defn year-week->iso-date
  "Create iso date from year and week"
  ([year week]
   (year-week->iso-date year week "UTC"))
  ([year week tz]
   (let [year (if (number? year)
                year
                (Integer/parseInt year))
         week (if (number? week) week (Integer/parseInt  week))]
     (tf/parse (tf/formatter (tf/formatters :weekyear-week) (get-tz tz))
               (str year "-W" (format "%02d" week))))))

(defn year-month->iso-date
  "Create iso date from year and week"
  ([year month]
   (year-month->iso-date year month "UTC"))
  ([year month tz]
   (let [year (if (number? year)
                year
                (Integer/parseInt year))
         month (if (number? month ) month (Integer/parseInt  month ))]
     (tf/parse (tf/formatter (tf/formatters :year-month) (get-tz tz))
               (str year "-" (format "%02d" month))))))


(defn year->epoch
  "convert a year to an epoch number"
  [y]
  (tc/to-long (year-week->iso-date y 1)))

(defn year-month->epoch
  "convert a year and month to an epoch number"
  [y m]
  (tc/to-long (year-month->iso-date y m)))

(defn year-week->epoch
  "convert a year and week to an epoch number"
  [y w]
  (tc/to-long (year-week->iso-date y w)))

(def year-duration  (- (year->epoch 2016) (year->epoch 2015)))
(def month-duration (- (year-month->epoch 2016 2)
                       (year-month->epoch 2016 1)))
(def week-duration  (- (year-week->epoch 2016 2)
                       (year-week->epoch 2016 1)))
(def day-duration   (- (str-date->epoch "01/02/2016")
                       (str-date->epoch "01/01/2016")) )
(def hour-duration  3600000)

(defn get-epoch-dispatch
  [year month week]
  (cond
    week :week
    month :month
    year :year
    :else :non))

(defmulti get-epoch
  "turn a year, year and month, or year and week into an epoch number"
  get-epoch-dispatch)

(defmethod get-epoch :year [year month week]
  (year->epoch year))

(defmethod get-epoch :month [year month week]
  (year-month->epoch year month))

(defmethod get-epoch :week [year month week]
  (year-week->epoch year week))

(defmethod get-epoch :non [year month week] 0)

(defn epoch-dispatch
  [year month week]
  (cond
    week :week
    month :month
    year :year
    :else :non))

(defn epoch+duration-dispatch [from year month week range]
  (epoch-dispatch year month week))

(defmulti epoch+duration
  "Given an epoch, year month or week and range, give an epoch
  number for the appropriate duration. default is 1 year, 1 month
  or 1 week depending on precedence, week, month and then year."
  epoch+duration-dispatch)

(defmethod epoch+duration :year [from year month week range]
  (+ from (* year-duration (or range 1))))

(defmethod epoch+duration :month [from year month week range]
  (+ from (* month-duration (or range 1))))

(defmethod epoch+duration :week [from year month week range]
  (+ from (* week-duration (or range 1))))

(defmethod epoch+duration :day [from year month week range]
  (+ from (* day-duration (or range 1))))

(defmethod epoch+duration :hour [from year month week range]
  (+ from (* hour-duration (or range 1))))

(defmethod epoch+duration :non [from year month week range]
  (println "to epoch: cannot convert."))

(defn epoch-range-dispatch [year month week range]
  (epoch-dispatch year month week))

(defmulti epoch-range
  "Given a year, month, week, and range give a from and a to epoch
  number for the appropriate range. "
  epoch-range-dispatch)

(defmethod epoch-range :week [year month week range]
  (let [from (year-week->epoch year week)]
    {:from from :to (epoch+duration from year month week range)}))

(defmethod epoch-range :month [year month week range]
  (let [from (year-month->epoch year month)]
    {:from from :to (epoch+duration from year month week range)}))

(defmethod epoch-range :year [year month week range]
  (let [from (year->epoch year)]
    {:from from :to (epoch+duration from year month week range)}))

(defmethod epoch-range :non [year month week range]
  {:from nil :to nil })

(defn epoch-between [from to epoch]
  (if (nil? from)
    true
    (and (>= epoch from) (< epoch to))))

(defn weeks-between [epoch1 epoch2]
  (t/in-weeks (t/interval (tc/from-long epoch1)
                          (tc/from-long epoch2))))

(defn extract-year [d]
  (t/year d))

(defn extract-week
  "Given an iso date, return the week number."
  [date]
  (t/week-number-of-year date))

(defn epoch->iso-date
  "Given an epoch and zone id or zone offset return an iso-date in that zone."
  ([epoch]
   (epoch->iso-date epoch "UTC"))
  ([epoch tz]
   (t/to-time-zone (tc/from-long epoch) (get-tz tz))))

(defn extract-date
  "Given an epoch number, an iso-date or a string date,
  or year and week, return a string representation
  of an iso date and integers for year and week."
  [& {:keys [iso-date str-date str-iso-date epoch year week month timezone]
      :or {timezone "UTC"}}]
  (let [epoch (cond
                (nil? epoch) nil
                (number? epoch) (long epoch)
                (string? epoch) (long (bigint epoch)))
        iso-date (cond
                   iso-date (t/to-time-zone iso-date (get-tz timezone))
                   epoch (epoch->iso-date (long epoch) timezone)
                   str-date (date->iso str-date timezone)
                   str-iso-date (str-iso-date->iso-date str-iso-date timezone)
                   (and year week) (year-week->iso-date year week timezone)
                   (and year month) (year-month->iso-date year month timezone)
                   year (date->iso (str year) timezone))
        year (if iso-date (t/year iso-date))
        month (if iso-date (t/month iso-date))
        week (if iso-date (t/week-number-of-year iso-date))
        epoch (if iso-date (tc/to-long iso-date))]
    {:date (str iso-date) :iso-date iso-date :year year :month month :week week :epoch epoch}))

(defn timestamp []
  (extract-date :iso-date (t/now)))

(defn epoch-timestamp
  "Return the number of milliseconds after the Unix epoch as a Long."
  []
  (tc/to-long (t/now)))

(def now t/now)

(defn now-str []
  (:date (extract-date :iso-date (t/now))))

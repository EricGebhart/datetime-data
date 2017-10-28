(ns org.eag.datetime-data.core-test
  (:require [clj-time.coerce :as tc]
            [midje.sweet :refer :all]
            [org.eag.datetime-data.core :refer :all]))

(defn str-date [iso-date]
  (:date (extract-date :iso-date iso-date)))

(defn str-to-epoch [str]
  (tc/to-long (date->iso str)))

(facts "`date->iso` returns a Datetime"
       (fact "It Can convert a date string"
             (type (date->iso "1980-04-24")) => org.joda.time.DateTime
             (type (date->iso "04-24-1980")) => org.joda.time.DateTime
             (type (date->iso "04-24-1980 10:30:00")) => org.joda.time.DateTime
             (type (date->iso "04-24-1980 22:30:00")) => org.joda.time.DateTime

             (str-date (date->iso "1980-04-24")) => "1980-04-24T00:00:00.000Z"
             (str-date (date->iso "1980-04-24 22:30:00")) => "1980-04-24T22:30:00.000Z"

             (str-date (date->iso "04-24-1980")) => "1980-04-24T00:00:00.000Z"
             (str-date (date->iso "04-24-1980 10:30:00")) => "1980-04-24T10:30:00.000Z"
             (str-date (date->iso "04-24-1980 22:30:00")) => "1980-04-24T22:30:00.000Z")

       (fact "It can convert an epoch number that is a long."
             (type (date->iso (str-date->epoch "1980-04-24"))) =>  org.joda.time.DateTime
             (str-date (date->iso (str-date->epoch "1980-04-24"))) => "1980-04-24T00:00:00.000Z")

       (fact "It can convert an epoch number that is a double."
             (type (date->iso 325400400000.0)) => org.joda.time.DateTime
             (str-date (date->iso 325382400000.0)) => "1980-04-24T00:00:00.000Z"
             (str-date (date->iso 325400400000.0)) => "1980-04-24T05:00:00.000Z"))

;; get the epoch number
;; (fact "clj-time.coerce/to-long can turn an iso date into an epoch long."
;; (tc/to-long (date-to-iso "1980-04-24")) => 325400400000)

(facts "extract date returns an iso date string, the year and week."
       ;; get an iso-date string, year and week from an epoch.
       (fact "It can take a an epoch number"
             (extract-date :epoch (str-date->epoch "1980-04-24"))
             => (contains {:date "1980-04-24T00:00:00.000Z", :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take an iso-date."
             (extract-date :iso-date (date->iso "1980-04-24"))
             => (contains {:date "1980-04-24T00:00:00.000Z", :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take a string date."
             (extract-date :str-date "1980-04-24")
             => (contains {:date "1980-04-24T00:00:00.000Z", :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take just a year."
             (extract-date :year 1980)
             => (contains {:date "1980-01-01T00:00:00.000Z", :year 1980, :week 1 :month 1}
                          :in-any-order :gaps-ok))

       (fact "It can take a year and a week."
             (extract-date :year 1980 :week 17)
             => (contains {:date "1980-04-21T00:00:00.000Z", :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok)))

;; convert an iso date string to an iso date.
(fact "`str-iso-date->iso-date converts between iso date objects and the representative string."
      (type (str-iso-date->iso-date (str-date (date->iso "04-24-1980")))) => org.joda.time.DateTime
      (str-date (str-iso-date->iso-date
                 (:date (extract-date :epoch (str-date->epoch "1980-04-24"))))) => "1980-04-24T00:00:00.000Z"
      (->> "1980-04-24"
           date->iso
           tc/to-long
           (extract-date :epoch)
           :date
           str-iso-date->iso-date
           str-date)
      => "1980-04-24T00:00:00.000Z")

(tabular
 (facts "check that an epoch number is between or equal to two other epoch numbers"
        (epoch-between ?from ?to ?this) => ?result)

 ?from ?to ?this ?result
 1      2   1    truthy
 1      2   2    falsey
 1      2   3    falsey
 1      5   4    truthy
 1      10   9    truthy
 1      2   0    falsey
 )


(facts "extract date returns values which can be adjusted for timezone , ."
       ;; get an iso-date string, year and week from an epoch.
       (fact "It can take a an epoch number"
             (extract-date :timezone "Europe/Warsaw"
                           :epoch (str-date->epoch "1980-04-24" "Europe/Warsaw"))
             => (contains {:date "1980-04-24T00:00:00.000+02:00",
                           :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take an iso-date."
             (extract-date :timezone "Europe/Warsaw"
                           :iso-date (date->iso "1980-04-24"))
             => (contains {:date "1980-04-24T02:00:00.000+02:00",
                           :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take a string date."
             (extract-date :timezone "Europe/Warsaw"
                           :str-date "1980-04-24")
             => (contains {:date "1980-04-24T00:00:00.000+02:00",
                           :year 1980, :week 17 :month 4}
                          :in-any-order :gaps-ok))

       (fact "It can take just a year."
             (extract-date :timezone "Europe/Warsaw" :year 1980)
             => (contains {:date "1980-01-01T00:00:00.000+01:00",
                           :year 1980, :week 1 :month 1}
                          :in-any-order :gaps-ok))

       (fact "It can take a year and a week."
             (extract-date :timezone "Europe/Warsaw" :year 1980 :week 17)
             => (contains
                 {:date "1980-04-21T00:00:00.000+02:00",
                  :epoch 325116000000,
                  :month 4, :week 17, :year 1980}
                 :in-any-order :gaps-ok)))


(fact "it can be turned into a timezone specific date time and back again."
      (extract-date :timezone "UTC"
                    :iso-date (:iso-date
                               (extract-date :timezone "Europe/Warsaw"
                                             :iso-date (date->iso "1980-04-24"))))
      =>
      (contains {:date "1980-04-24T00:00:00.000Z",
                 :year 1980, :week 17 :month 4}
                :in-any-order :gaps-ok))

(fact "An epoch number has no timezone information and
       reflects the original time given without the timezone shift."
      (extract-date :timezone "UTC"
                    :epoch (:epoch
                            (extract-date :timezone "Europe/Warsaw"
                                          :iso-date (date->iso "1980-04-24"))))
      =>
      (contains {:date "1980-04-24T00:00:00.000Z",
                 :year 1980, :week 17 :month 4}
                :in-any-order :gaps-ok))

(fact "An epoch number has no timezone information and
       reflects the original time given without the timezone shift.
       So setting the timezone on iso-date creation will create the
       correct date with timezone shift.
"
      (extract-date :timezone "Europe/Warsaw"
                    :epoch (:epoch
                            (extract-date :timezone "Europe/Warsaw"
                                          :iso-date (date->iso "1980-04-24"))))
      =>
      (contains {:date "1980-04-24T02:00:00.000+02:00",
                 :year 1980, :week 17 :month 4}
                :in-any-order :gaps-ok))

(fact "An iso-date knows it's timezone and will keep things straight."
      (extract-date :timezone "Europe/Warsaw"
                    :iso-date (:iso-date
                               (extract-date :timezone "Europe/Warsaw"
                                             :iso-date (date->iso "1980-04-24"))))
      =>
      (contains {:date "1980-04-24T02:00:00.000+02:00",
                 :year 1980, :week 17 :month 4}
                :in-any-order :gaps-ok))

(fact "Converting from a string iso date also keeps everything straight."
      (extract-date :timezone "Europe/Warsaw"
                    :str-iso-date (:date
                                   (extract-date :timezone "Europe/Warsaw"
                                                 :iso-date (date->iso "1980-04-24"))))
      =>
      (contains {:date "1980-04-24T02:00:00.000+02:00",
                 :year 1980, :week 17 :month 4}
                :in-any-order :gaps-ok))

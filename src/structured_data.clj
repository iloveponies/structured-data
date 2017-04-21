(ns structured-data
  (:require [clojure.set :as set]))

(defn do-a-thing [x]
  (let [twice (+ x x)]
    (Math/pow twice twice))
  )

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
  (- x2 x1))
  )

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [x y] point]
    (and (<= rx1 x rx2) (<= ry1 y ry2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[outer-x1 outer-y1] [outer-x2 outer-y2]] outer
        [[inner-x1 inner-y1] [inner-x2 inner-y2]] inner]
    (and (<= outer-x1 inner-x1 inner-x2 outer-x2)
         (<= outer-y1 inner-y1 inner-y2 outer-y2)
         )
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
    )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [coll] (get coll 1))]
    (map second collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if birth-year
      (apply str [name " (" birth-year " - " death-year ")"])
      name)
    )
  )

(defn authors->string [authors]
  (let [names (map author->string authors)]
    (apply str (interpose ", " names))
    )
  )

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
    (str (:title book) ", written by " authors))
  )

(defn books->string [books]
  (let [book-count (count books)
        book-strings (apply str (interpose ", " (map book->string books)))]
    (cond
      (= 0 book-count) "No books."
      (= 1 book-count) (str "1 book. " book-strings ".")
      :else (str book-count " books. " book-strings ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living-authors (living-authors authors)]
    (not (empty? living-authors))
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%

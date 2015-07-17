(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a0 a1 a2] v]
    (+ a0 a2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
    (- y2 y1))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
  (and (contains-point? outer inner-bottom-left)
       (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [new-author-list (conj (book :authors) new-author)]
  (assoc book :authors new-author-list)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [item] (get item 1))]
        (map get-second collection)))

(defn titles [books]
 (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq) true (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn has-authorchina? [author book]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
   (set (map :name (apply clojure.set/union (map :authors books)))))

(defn author->string [writer]
  (let [name (fn [author] (author :name))]
    (let [bornyear (fn [author] (author :birth-year))]
      (let [deathyear (fn [author] (author :death-year))]
        (apply str (name writer)
          (if (:birth-year writer)
            (apply str " (" (bornyear writer) " - " (deathyear writer) ")")
            (str "")))))))



(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
 (apply str (:title book) ", written by " (authors->string (:authors book)))
 )





(defn books->string [books]
  (let [num (count books)]
    (if (== num 0) "No books."
      (apply str (str num) " book" (if (> num 1) "s" "") ". "
        (apply str (interpose ". " (map book->string books))) "."))))

(defn equal-strings? [string1 string2]
  (let [len1 (count string1)]
    (let [len2 (count string2)]
     (cond
       (== 0 len1 len2) true
       (not (== len1 len2)) false
        :else      (equal-strings? (rest string1) (rest string2))
          ))))


(defn books-by-author [author books]
  (let [has-known-aut? (fn [book] (has-author? book author))]
    (filter has-known-aut? books)))

(defn getauthorlist [authors]
  (map :name (filter :name authors))
  )




(defn author-by-name [autname authors]
  (if (equal-strings? (first (getauthorlist authors)) autname)
    (first (filter :name authors))
    (if (> (count (rest authors)) 0)
      (author-by-name autname (rest authors))
      nil)
  )
)

(defn living-authors [authors]
  (let [is-alive? (fn [author] (not (contains? author :death-year)))]
    (filter is-alive? authors)
  ))

(defn has-a-living-author? [book]
  (let [lives? (fn [author] (not (contains? author :death-year)))]
  (contains? (set (map lives? (:authors book))) true )))



(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


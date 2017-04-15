(ns structured-data)

(defn do-a-thing [x]
  (let [x x]
    (Math/pow (+ x x) (+ x x)))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[o k e] v]
    (+ o e)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bl1 tr1] [bl2 tr2]] rectangle]
  (- bl2 bl1)))

(defn height [rectangle]
  (let [[[bl1 tr1] [bl2 tr2]] rectangle]
  (- tr2 tr1)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (if (and (<= (get (first rectangle) 0) (get point 0) (get (second rectangle) 0) )
    (<= (get (first rectangle) 1) (get point 1) (get (second rectangle) 1) ) ) true false))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and
      (contains-point? outer inner-bottom-left)
      (contains-point? outer inner-top-right))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors
  (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (get author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (map second (seq collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq))
           (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (fn [kirja] (get kirja :authors))]
  (assoc book :authors (set (authors book)))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
(apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (get author :birth-year)
        death (get author :death-year)
        name  (get author :name)]
    (str name
      (if birth
        (str " (" birth " - "
          (if death
            (str death ")")
            ")"))
        ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
    (let [infos (apply str (interpose ", " (map book->string books)))
        num   (count books)]
    (str
      (cond
        (== num 0) "No books"
        (== num 1) "1 book. "
        :else (str num " books. "))
      infos ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
 (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

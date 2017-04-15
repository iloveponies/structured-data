(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
;if too short, it returns nil (I guess, emacs repl plugin is broken)
  (+
   (get v 0)
   (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- a c))))

(defn height [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- b d))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b] [c d]] rectangle
        [x y] point]
    (if (and (<= a x c) (<= b y d))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (if (and (contains-point? outer a) (contains-point? outer b))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (= (get author :death-year) nil)
    true
    false))

(defn element-lengths [collection]
  (map (fn [v] (count v)) collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (map sec collection)))

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
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")")
                "")]
    (str (:name author) years)))

(defn authors->string [authors]
  (let [authlist (map author->string authors)]
    (if (> (count authors) 1)
      (seq (interpose ", " authlist))
      (if (> (count authors) 0)
        (first authlist)
        ""))))

(defn book->string [book]
  (str (:title book) ", written by " (apply str (authors->string (:authors book)))))

(defn books->string [books]
  (cond
   (== (count books) 1) (str "1 book. " (book->string (first books))".")
   (> (count books) 1) (str (count books) " books. " (apply str (interpose ", "
                                                        (map book->string
                                                             books)))
                            ".")

   :else "No books."))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [isnil (living-authors (:authors book))]
    (if (== (count isnil) 0)
      false
      true)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%

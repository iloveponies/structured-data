(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x )]
    (Math/pow a a)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))


(defn cutify [v] (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))


(defn square? [rectangle]
    (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle] (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (and (<= x1 a x2) (<= y1 b y2))))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    ( and (contains-point? outer a) (contains-point? outer b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [book (assoc book :authors (conj (book :authors) new-author))] book))

(defn alive? [author] (-> author :death-year boolean not))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [toka (fn [x] (get x 1))]
    (map toka collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq)
  )
)

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem)
                            (disj a-set elem)
                            (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  ( > (count a-seq)(count (set a-seq))))


(defn old-book->new-book [book]
  (let [book (assoc book :authors (set (book :authors)))] book))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

 (defn author->string [author]
  (let [life (when(:birth-year author)(str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str (:name author) life)
))

(defn authors->string [authors]
  (apply str (interpose ", " (set (map author->string authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (str(authors->string(:authors book)))))


(defn books->string [books]
  (str (cond
    (== (count books) 0) "No books"
    (== (count books) 1) "1 book. "
    :else (str (count books) " books. "))
  (apply str (interpose ". " (seq (map book->string books))))"."))

(defn books-by-author [author books]
   (filter (fn [book](has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter #(= (:death-year %) nil) authors))

(defn has-a-living-author? [book]
  (> (count (filter #(= (:death-year %) nil) (:authors book ))) 0 ))

(defn books-by-living-authors [books]
   (filter #(has-a-living-author? %)books))

; %________%

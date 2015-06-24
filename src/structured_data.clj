(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [eka (get v 0)
        toka (get v 2)]
    (+ eka toka)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a1 b1] point]
    (and (<= x1 a1 x2) (<= y1 b1 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[a1 b1] [a2 b2]] inner]
        (and (<= x1 a1 x2) (<= y1 b1 y2) (<= x1 a2 x2) (<= y1 b2 y2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (let [maara (count (get book :authors))]
    (> maara 1)))

(defn add-author [book new-author]
  (let [kirjoittajat (conj (get book :authors) new-author)]
    (assoc book :authors kirjoittajat)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
   (map second-element collection)))

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
  (let [size1 (count a-seq)
        size2 (count (set a-seq))]
    (> size1 size2)))

(defn old-book->new-book [book]
  (let [kartta (:authors book)]
    (assoc book :authors (set kartta))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author (fn [book] (:authors book))]
  (apply clojure.set/union (map author books))))

(defn all-author-names [books]
  (let [all-author  (authors books)]
    (set (map :name all-author))))

(defn author->string [author]
  (let [name (:name author)
        dates (cond
               (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
               (contains? author :birth-year) (str " (" (:birth-year author) " - " ")"))]
    (str name dates)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))


(defn books->string [books]
  (let [lkm (count books)
        book-strings (apply str (interpose ". " (map book->string books)))]
    (cond
     (<= lkm 0) (str "No books.")
     (= lkm 1) (str "1 book. " book-strings ".")
     :else (str lkm " books. " book-strings "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%

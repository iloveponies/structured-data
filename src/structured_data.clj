(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
    (+ a c))

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

(defn square? [[[x1 x2] [y1 y2]]]
  (= (- x1 y1) (- x2 y2)))

(defn area [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
    (* (- y1 x1) (- y2 x2))))

(defn contains-point? [rectangle point]
  (let [[[x1 x2] [y1 y2]] rectangle
        [z1 z2] point]
    (and (<= x1 z1 y1)
         (<= x2 z2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 x2] [y1 y2]] outer [[z1 z2] [n1 n2]] inner]
    (and (<= x1 z1)
         (<= x2 y2)
         (>= y1 n1)
         (>= y2 n2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [second (fn [[x y]] y)]
    (map second collection)))

(defn titles [books]
  (let [titles (fn [book] (:title book))]
    (map titles books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (== (count a-set) (count a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        auth-set (set authors)]
    (assoc book :authors auth-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book (map (fn [book] (:authors book)) books)]
    (apply clojure.set/union book)))

(defn all-author-names [books]
  (let [authors (authors books)]
    (set (map (fn [book] (:name book)) authors))))

(defn author->string [author]
  (let [a-name (str (:name author))]
    (if (contains? author :birth-year)
      (str a-name " (" (:birth-year author) " - " (:death-year author) ")")
      a-name)))

(defn authors->string [authors]
  (let [s-auth (map author->string authors)]
    (apply str (interpose ", " s-auth))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [names (apply str (interpose ". " (map book->string books)))]
    (cond 
      (== (count books) 0) "No books."
      (== (count books) 1) (str "1 book. " names ".")
      :else (str (count books) " books. " names "."))))

(defn books-by-author [author books]
  (filter 
    (fn [book] (has-author? book author))
     books))

(defn author-by-name [name authors]
  (let [author (filter
                (fn [author] (= name (:name author)))
                authors)]
    (if (== 0 (count author))
      nil
      (first author))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

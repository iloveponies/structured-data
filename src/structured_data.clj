(ns structured-data)

(defn do-a-thing [x] (let [y (+ x x)]
  (Math/pow y y)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[x y z] v]
                                (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
                          (- x2 x1)))

(defn height [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
                          (- y2 y1)))

(defn square? [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
                          (== (- x2 x1) (- y2 y1))))

(defn area [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
                          (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point] (let [[[x1 y1] [x2 y2]] rectangle]
                          (and (<= x1 (get point 0) x2) (<= y1 (get point 1) y2))))


(defn contains-rectangle? [outer inner] (let [[[r1x1 r1y1] [r1x2 r1y2]] outer
                                       [[r2x1 r2y1] [r2x2 r2y2]] inner]
                                  (and (<= r1x1 r2x1) (<= r1y1 r2y1) (>= r1x2 r2x2) (>= r1y2 r2y2))))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (< 1 (count (:authors book))))

(defn add-author [book new-author] (let [original book
      updated (assoc original :authors (conj (:authors original) new-author))]
  updated))

(defn alive? [author] (not (contains? author :death-year)))

(defn element-lengths [collection] (map (fn [x] (count x)) collection))

(defn second-elements [collection] (let [second-el (fn [x] (get x 1))]
                                     (map second-el collection)))

(defn titles [books] (map :title books))

(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq] (if (== (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book] (assoc book :authors (set (book :authors))))

(defn has-author? [book author] (contains? (book :authors) author))

(defn authors [books] (apply clojure.set/union (map :authors books)))

(defn all-author-names [books] (set (map :name (apply clojure.set/union (map :authors books)))))

(defn author->string [author] (let [aname (:name author)
      byear (:birth-year author)
      dyear (:death-year author)]
  (str aname (if byear
               (if dyear (str " (" (str byear) " - " dyear ")") (str " (" byear " - )"))
               nil))))

(defn authors->string [authors] (apply str (interpose ", " (map author->string authors))))

(defn book->string [book] (let [btitle (:title book)
       bauthors (:authors book)]
      (str btitle ", written by " (authors->string bauthors))))

(defn books->string [books] (let [bcount (count books)]
                              (if (== bcount 0)
                                (str "No books.")
                                (if (== bcount 1)
                                  (str bcount " book. " (book->string (get books 0)) ".")
                                  (str bcount " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books] (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors] (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors] (filter (fn [x] (if (:death-year x) false true)) authors))

(defn has-a-living-author? [book] (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books] (filter (fn [x] (has-a-living-author? x)) books))

; %________%

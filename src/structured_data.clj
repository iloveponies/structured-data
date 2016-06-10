(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0 0) (get v 2 0)))

(defn spiff-destructuring [[a b c]]
  (+ (or a 0) (or c 0)))

(defn cutify [v]
  (conj v "<3"))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner]
    (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj  a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [_authors (map :authors books)]
    (apply clojure.set/union _authors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (get author :name)
        from (get author :birth-year)
        till (get author :death-year)]
    (str name (if from (str " (" from " - " till ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        authors (get book :authors)
        by (if-not (empty? authors)
             (str ", written by " (authors->string authors)))]
    (str title by)))

(defn books->string [books]
  (let [nrofbooks (count books)
        qualifier (if (= nrofbooks 1) "book. " "books. ")]
    (if (empty? books)
      "No books."
      (str nrofbooks " " qualifier (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter #(has-author? %1 author) books))

(defn author-by-name [name authors]
  (let  [predicate #(= name (:name %1))
         result (filter predicate authors)]
    (if (empty? result) nil (first result))))

(defn living-authors [authors]
  (filter #(not (get %1 :death-year)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %1) books))

; %________%

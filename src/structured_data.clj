(ns structured-data
  (:use clojure.repl))

(defn do-a-thing [x]
  (let [x2 (+ x x)]
  (Math/pow x2 x2)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x? y?] point]
    (and (<= x1 x? x2)
         (<= y1 y? y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] outer
        [[x3 y3][x4 y4]] inner]
    (and (<= x1 x3 x4 x2)
         (<= y1 y3 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [old-author (:authors book)]
    (assoc book :authors
      (conj old-author new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [[_ x2]] x2)]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [count-uniq (fn [a-seq] (count (set a-seq)))]
    (not= (count a-seq) (count-uniq a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [authors-seq (map :authors books)]
  (apply clojure.set/union authors-seq)))

(defn all-author-names [books]
  (let [authors-uniq (authors books)]
  (set (map :name authors-uniq))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if (:birth-year author)
      (str name " (" birth-year " - " death-year ")")
      (str name))))

(defn authors->string [authors]
  (let [authors-str (map author->string authors)]
  (apply str (interpose ", " authors-str))))

(defn book->string [book]
  (let [title (:title book)
        authors-set (:authors book)
        authors-str (authors->string authors-set)]
  (str title ", written by " authors-str)))

(defn books->string [books]
  (let [book-count (count books)
        books-str (apply str (interpose ". " (map book->string books)))]
    (if (= book-count 0) "No books."
      (str book-count
           (if (= book-count 1) " book. " " books. ")
           books-str "."))))

(defn books-by-author [author books]
  (filter (fn [a-book] (has-author? a-book author)) books))

(defn author-by-name [name authors]
  (let [eq-author (fn [an-author] (= name (:name an-author)))]
  (first (filter eq-author  authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

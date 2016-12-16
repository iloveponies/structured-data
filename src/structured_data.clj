(ns structured-data)

(defn do-a-thing [x]
(let [x* (+ x x)]
  (Math/pow x* x*)))
(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
      (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (= (height rectangle) (width rectangle))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [pointx pointy] point]
    (and (<= x1 pointx x2)
         (<= y1 pointy y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [author-list (:authors book)]
      (assoc book :authors
        (conj author-list new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn[vector] (get vector 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq )
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq))
     (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (= (count authors)
       (count (conj authors author)))))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
    (let [author-name (:name author)
          birth (:birth-year author)
          death (:death-year author)
          years (if birth (str " (" birth " - " death ")" ))]
    (str author-name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-name (:title book)
        authors (authors->string (:authors book))]
    (str book-name ", written by " authors)))

(defn books->string [books]
  (let [numBooks (count books)
        bookStr (str " book" (if (> numBooks 1) "s. " ". "))
        authors (apply str (interpose ". "(map book->string books)))]
    (if (= numBooks 0)
      "No books."
      (str numBooks bookStr authors "."))
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))
          books))

(defn author-by-name [name authors]
  (first (filter
           (fn [author] (= name (:name author)))
            authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living-authors (living-authors authors)]
    (not (empty? living-authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

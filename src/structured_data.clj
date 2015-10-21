(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn
   cutify
   [v]
    (conj v "<3"))



(defn
  spiff-destructuring
  [v]
    (let [[x y z] v]
      (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false
    ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))
      true
      false
      )))





(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author
  [book new-author]
     (assoc book :authors (assoc (:authors book) (author-count book) new-author)))



(defn alive?
  [author]
  (if (= (:death-year author) nil)
    true
    false))


(defn element-lengths
   [collection]
  (map count collection))


(defn second-elements
  [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))


(defn titles
  [books]
  (map :title books))


(defn monotonic?
  [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))



(defn stars
  [n]
  (apply str (repeat n "*")))


(defn toggle
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates?
  [sequence]
  (not (= (count sequence) (count (set sequence)))))


(defn old-book->new-book
  [book]
  (assoc book :authors (set (:authors book))))

(defn has-author?
  [book author]
  (contains? (:authors book) author))



(defn authors
  [books]
  (set(clojure.set/union (apply concat (map :authors books)))))



(defn all-author-names
  [books]
  (set (map :name (authors books))))

(defn author->string
  [author]
  (let [[name birth death] [(:name author) (:birth-year author) (:death-year author)]]
    (cond
     (= (count author) 1) name
     (= (count author) 2) (str name " (" birth " - )")
     (= (count author) 3) (str name " (" birth " - " death ")"))))

(defn authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string
  [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string
  [books]
  (cond
   (= (count books) 0) "No books."
   (= (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
   (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))


(defn books-by-author
  [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name
  [name authors]
  (def solution (filter (fn [author] (= name (:name author))) authors))
  (if (= (count solution) 0)
    nil
    (first solution)))

(defn living-authors
  [authors]
  (filter (fn [author] (alive? author)) authors))



(defn has-a-living-author?
  [book]
  (def solution (filter (fn [author] (alive? author)) (:authors book)))
  (if (= (count solution) 0)
    false
    true))

(defn books-by-living-authors
  [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%

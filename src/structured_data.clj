(ns structured-data)


(defn do-a-thing[x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn
  spiff
  [v]
  (+ (get v 0)(get v 2)))

(defn
  cutify
  [v]
  (conj v "<3"))


(defn
  spiff-destructuring
  [v]
  (let [[x y z] v]
  (+ x z)))


(defn
  point
  [x y]
  [x y])


(defn
  rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  [rectangle]
  (let [[[x1 y1]
        [x2 y2]] rectangle]
    (- x2 x1)))


(defn height
  [rectangle]
  (let [[[x1 y1]
        [x2 y2]] rectangle]
        (- y2 y1)))

(defn
  square?
  [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))


(defn
  area
  [rectangle]
  (* (height rectangle) (width rectangle)))

(defn
  contains-point?
  [rectangle point]
  (let [[x y] point]
    (let [[[x1 y1]
          [x2 y2]] rectangle]
      (if (and (<= x1 x x2)
               (<= y1 y y2))
        true
        false))))


(defn
  contains-rectangle?
  [outer inner]
  (let [[p p1] inner]
    (if (and (contains-point? outer p)
             (contains-point? outer p1))
      true
      false)))


(defn
  title-length
  [book]
  (count (:title book)))


(defn
  author-count
  [book]
  (count (:authors book)))

(defn
  multiple-authors?
  [book]
  (if (> (author-count book) 1)
    true
    false))

(defn
  add-author
  [book new-author]
  (let [original (:authors book)
        new (conj original new-author)]
    (assoc book :authors new)))


(defn
  alive?
  [author]
 (not (contains? author :death-year)))

(defn
  element-lengths
  [collection]
  (map count collection))

(defn
  second-elements
  [collection]
  (let [vectoring (fn [v] (get v 1))]
    (map vectoring collection)))


(defn
  titles
  [books]
  (map :title books))


(defn
  monotonic?
  [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn
  stars
  [n]
  (apply str (repeat n \*)))


(defn
  toggle
  [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))


(defn
  contains-duplicates?
  [a-seq]
  (< (count (set a-seq)) (count a-seq)))


(defn
  old-book->new-book
  [book]
  (assoc book :authors
    (set (:authors book))))


(defn
  has-author?
  [book author]
  (contains? (:authors book) author))


(defn
  authors
  [books]
(set (apply concat (clojure.set/union
 (map :authors books)))))

(defn
  all-author-names
  [books]
  (set (map :name (authors books))))


(defn
  author->string
  [author]
  (let [nimi (:name author)
        svuosi (:birth-year author)
        kvuosi (:death-year author)]
    (if (contains? author :birth-year)
      (str nimi " (" svuosi " - " kvuosi \) )
      (str nimi))))


(defn
  authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn
  book->string
  [book]
   (let [kansi (:title book)
       kirjailija (authors->string (:authors book))]
  (str kansi ", written by " kirjailija)))


(defn
  books->string
  [books]
  (if (< 0 (count books))
  (str (apply str (count books)
         (if (= 1 (count books)) " book. "
           " books. ")
         (interpose ". " (map book->string books))) ".")
    (str "No books.")))


(defn
  books-by-author
  [author books]
  (filter
   (fn [book]
     (has-author? book author)) books))

(defn
  author-by-name
  [name authors]
  (first
    (filter
    (fn [author] (= (:name author) name))
    authors)))


(defn
  living-authors
  [authors]
  (filter
    alive? authors))


(defn
  has-a-living-author?
  [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))


(defn
  books-by-living-authors
  [books]
  (filter
    has-a-living-author?
    books))

; %________%

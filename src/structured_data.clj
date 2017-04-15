(ns structured-data)

;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn
  do-a-thing
  [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn
  spiff
  [v]
  (if (< (count v) 3)
    \?
    (+ (get v 0) (get v 2))
  ))


(defn
  cutify
  [v]
  (conj v "<3"))


(defn
  spiff-destructuring
  [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn
  point
  [x y]
  [x y])

(defn
  rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  abs
  [x]
  (if (< x 0)
    (* x -1)
    x))

(defn
  width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (abs (- x1 x2))))


(defn
  height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (abs (- y1 y2))))



(defn
  square?
  [rectangle]
    (if (== (height rectangle) (width rectangle))
      true
      false))


(defn
  area
  [rectangle]
  (* (height rectangle) (width rectangle)))



(defn
  contains-point?
  [rectangle point]
  (let [[[left_x bottom_y] [right_x top_y]] rectangle]
    (let [[point_x point_y] point]
      (if (<= left_x point_x right_x)
        (if (<= bottom_y point_y top_y)
          true
          false)
        false)
      )
  ))



(defn
  contains-rectangle?
  [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (if (and (contains-point? outer inner-bottom-left) (contains-point? outer inner-top-right))
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
  (assoc book :authors (conj (:authors book) new-author)))



(defn
  alive?
  [author]
  (if (contains? author :death-year)
    false
    true))



(defn
  element-lengths
  [collection]
  (map count collection))



(defn
  second-elements
  [collection]
  (let [jotain
        (fn [vector] (get vector 1))]
    (map jotain collection
  )))


(defn
  titles
  [books]
  (map :title books))



(defn
  monotonic?
  [a-seq]
  (if (apply <= a-seq)
    true
    (if (apply >= a-seq)
      true
      false)))



(defn
  stars
  [n]
  (apply str (repeat n "*")))



(defn
  toggle
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )



(defn
  contains-duplicates?
  [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))




(defn
  old-book->new-book
  [book]
  (assoc book :authors (set (:authors book))))




(defn
  has-author?
  [book author]
  (if (contains? (:authors book) author )
    true
    false))


(defn
  authors
  [books]
  (apply clojure.set/union (map :authors books)))



(defn
  all-author-names
  [books]
  (set (map :name (authors books))))




(defn author->string [author]
  (let [name
        (fn [author] (:name author))]
    (let [year
         (fn [author] (if (contains? author :birth-year)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")))]

    (str (name author) (year author)))))





(defn
  authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors)))
  )



(defn
  book->string
  [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )


(defn
  books->string
  [books]
  (if (= (count books) 0)
    (str "No books.")
    (if (= (count books) 1)
     (str (apply str "1 book. " (map book->string books)) ".")
     (str (apply str (count books) " books. " (interpose ". " (map book->string books))) "."))
  ))



(defn
  books-by-author
  [author books]
  (filter (fn [book] (has-author? book author)) books)
  )



(defn
  author-by-name
  [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )


(defn
  living-authors
  [authors]
  (filter alive? authors))


(defn
  has-a-living-author?
  [book]
  (not (empty? (living-authors (:authors book)))))

(defn
  books-by-living-authors
  [books]
  (filter has-a-living-author? books))


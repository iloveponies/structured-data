(ns structured-data)

(defn do-a-thing [x]
; bind the function to xplus
  (let [xplus (+ x x)]
    (Math/pow xplus xplus)))

(defn spiff [v]
;add the first element of v with the third element
  (+ (get v 0) (get v 2)))

(defn cutify [v]
;add a smiley to the end of the vector
  (conj v "<3"))

(defn spiff-destructuring [v]
;destructure the vector to elements and add a & c, 1st & 3rd
  (let [[a b c d] v]
    (+ a c)))

(defn point [x y]
;return a vector in function form
  [x y])

(defn rectangle [bottom-left top-right]
;define a vector in function form
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle 
                  [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) 
          (contains-point? outer p2)
    )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  ; map :authors in book to authors
  (let [authors (:authors book)]
    ;add new author to authors list then reassociate with the book object
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
;logically reverse whether author contains the key :death-year
  (not (contains? author :death-year)))

(defn element-lengths [collection]
; apply changes func [arg arg arg] to (func arg arg arg)
  (map count collection))

(defn second-elements [collection]
  (let [nd (fn [x] (get x 1))]
    (map nd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
; apply changes func (arg arg arg) to (func arg arg arg
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
; apply changes func (arg arg arg) to (func arg arg arg)
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  ;if contains returns true the remove from set otherwise add to it
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
;compare the numbers in a list with the numbers in a deduped set
  (not (= (count a-seq) (count (set a-seq))
   )))

(defn old-book->new-book [book]
;extract the authors, convert to set, re-assoc with the old keyword
  (assoc book :authors 
   (set 
    (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set  
          (map :name (authors books))
  )
)

(defn author->string [author]
  (let [name (:name author) 
      years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (= years " ( - )")
      (str name)
      (str name years))
))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
(let [num (count books)]
  (if (= num 0)
    (str "No books.")
    (if (= num 1)
      (str "1 book. " (book->string (first books)) ".")
      (str num " books. " 
           (apply str (interpose ". " (map book->string books)))
           ".")
      )
    )
))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] 
            (= (:name author) name))
          authors))
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
  )

(defn spiff [v]
  (cond
    (<= 3 (count v)) (+ (get v 0) (get v 2))
    :else         "?")
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (+ (get v 0) (get v 2))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
 (let [[[a b][c d]] rectangle]
   ( - c a )
  )
  )

(defn height [rectangle]
   (let [[[a b][c d]] rectangle]
   ( - d b )
   )
)

(defn square? [rectangle]
   (= (height rectangle) (width rectangle) )
)

(defn area [rectangle]
 ( * (height rectangle) (width rectangle) )
  )

(defn contains-point? [rectangle point]
   (let [[[a b][c d]] rectangle
         [x y] point]
      (and ( <= a x c ) ( <= b y d ))
   )
)

(defn contains-rectangle? [outer inner]
 (let [[[inA inB][inC inD]] inner
       [[outA outB][outC outD]] outer]

  (and (contains-point? (rectangle [outA outB] [outC outD]) ;bottom-left
                 (point inA inB))
       (contains-point? (rectangle [outA outB] [outC outD]) ;top-right
                 (point inC inD))
   )

   )
)
(defn title-length [book]
   (let [bookTitle (:title book)]
     (count bookTitle);
     )
  )

(defn author-count [book]
    (count (:authors book));
  )

(defn multiple-authors? [book]
  (<= 2 (author-count book))
  )

(defn add-author [book new-author]
  (let [originalbook book
      newbook      (assoc originalbook :authors new-author)]
    (assoc newbook :authors (conj(:authors originalbook) (:authors newbook)))
  )
)

(defn alive? [author]
   ; (not (has-deathyear? author))
   (not (contains? author :death-year))
 )

;;helper functions
(defn has-birthyear? [author]
    (contains? author :birth-year)
 )

(defn has-deathyear? [author]
    (contains? author :death-year)
 )
;;helper functions

(defn element-lengths [collection]
   ;(mungefy collection)

  (let [munge (fn [x] (count x))]
    (map munge collection))
)

(defn second-elements [collection]
   ;(mungefy collection)
  (let [munge (fn [x] (get x 1))]
    (map munge collection))
)

(defn mungefy [a-seq]
  (let [munge (fn [x] (count x))]
    (map munge a-seq))
  )

(defn titles [books]
     (map :title books)
  )

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq)
  )
)

(defn stars [n]
(apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
   (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  ;if the count of distinct elements and all elements in a sequence is not equal sequence contains duplicates
  (not= (count (distinct a-seq)) (count a-seq))
  )

(defn old-book->new-book [book]
   (assoc book :authors (set (:authors book)))
  ;(assoc book :authors (apply concat (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books))))
  )

(defn authors [books]
    (set (apply concat (map :authors books)))
  )

(defn author->string [author]
       (cond (has-birthyear? author) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")" )
              :else (str (:name author))
              )
  )

(defn authors->string [authors]
 (apply str (interpose ", "  (map author->string authors)))
  )

(defn book->string [book]
   ( str (:title book) ", written by "  (authors->string (:authors book))
     )
  )

(defn books->string [books]
 (str
      (cond (== 0 (count books)) "No books"
            (== 1 (count books)) (str (count books) " book. ")
            (< 1 (count books))  (str (count books) " books. ")
            )
      (apply str (interpose ". "  (map book->string books))) "."
      )
  )

(defn books-by-author [author books]
 (filter #(has-author? % author) books)
)
(defn author-by-name [name authors]
  (first (set (cond (empty?  (filter (fn [x] (=  (:name x) name))  authors)) nil
        :else (filter (fn [x] (=  (:name x) name))  authors)
        )

  )
)
  )
(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )
; %________%

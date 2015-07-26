(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  ))
; 3

(defn spiff [v]
  (+ (get v 0) (get v 2)))
; 5

(defn cutify [v]
  (conj v "<3"))
;8

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))
;10

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
;20

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))
;28

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))
;34

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
  ))
;50

(defn contains-rectangle? [outer inner]
  (let [[bottom-left-inner top-right-inner] inner]
        (and (contains-point? outer bottom-left-inner)
             (contains-point? outer top-right-inner))
   ))
;56

(defn title-length [book]
  (count (:title book)))
;59

(defn author-count [book]
  (count (:authors book)))
;62

(defn multiple-authors? [book]
  (< 1 (author-count book)))
;64

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))
;66

(defn alive? [author]
  (not (contains? author :death-year)))
;68

(defn element-lengths [collection]
  (map count collection))
;71

(defn second-elements [collection]
  (let [second-helper (fn [col] (get col 1))]
   (map second-helper collection)))
;73

(defn titles [books]
  (map :title books))
;75

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))
;83

(defn stars [n]
  (apply str (repeat n "*")))
;78

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))
;85

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
       (< (count a-set) (count a-seq))))
;89

(defn old-book->new-book [book]
  (let [authors (:authors book)]
      (assoc book :authors (set authors))))
;92

(defn has-author? [book author]
  (let [book-set (old-book->new-book book)]
    (contains? (:authors book-set) author)))
;97

(defn authors [books]
  (let [book-sets (map old-book->new-book books)]
      (apply clojure.set/union (map :authors book-sets))))
;100

(defn all-author-names [books]
  (set (map :name (authors books))))
;103

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if birth-year
      (if death-year
        (str name " (" birth-year " - " death-year ")")
        (str name " (" birth-year " - )"))
      name)))
;106


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))
;108

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))
;109

(defn books->string [books]
  (let [bookList (apply str (interpose ". " (map book->string books)))]
   (if (<= 1 (count books))
    (if (= 1 (count books))
      (str "1 book. " bookList ".")
      (str (count books) " books. " bookList "."))
    "No books.")))
;114

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))
;116

(defn author-by-name [name authors]
  (let [matches (filter (fn [author] (= (:name author) name)) authors)]
    (if (= 0 (count matches))
      nil
      (first matches))))
;118

(defn living-authors [authors]
  (filter alive? authors))
;121

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))
;126

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))
;131

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x0 x1 x2] v]
    (+ x0 x2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let[[[x0 _][x1 _]] rectangle]
    (Math/abs (- x1 x0))))

(defn height [rectangle]
  (let[[[_ y0][_ y1]] rectangle]
    (Math/abs(- y1 y0))))

(defn square? [rectangle]
  (= 0 (- (width rectangle) (height rectangle))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let[[[x0 y0] [x1 y1]] rectangle [px py] point]
    (and
     (or (<= x0 px x1) (<= x1 px x0)) 
     (or (<= y0 py y1) (<= y1 py y0)))))

(defn contains-rectangle? [outer inner]
  (let[[p0 p1] inner]
   (and (contains-point? outer p0) (contains-point? outer p1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1 ))

(defn add-author [book new-author]
  ( let [authors (conj (:authors book) new-author)]
  (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [author-set (set(:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-map (map :authors books)]
  (set (apply concat author-map))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        years (cond
                (= [birth death] [nil nil]) ""
                :else (str " (" birth " - " death ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str( interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [bookCount (count books)
        bookStr (cond
                  (== bookCount 0) "No books"
                  (== bookCount 1) "1 book. "
                  :else (str bookCount " books. "))]
    (str bookStr (apply str (interpose ". " (map book->string books)))".")))
          
(defn books-by-author [author books]
  (filter (fn[book] (has-author? book author)) books))
 
(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

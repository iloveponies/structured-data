(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)))

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
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [x y] point]
    (and (<= rx1 x rx2) (<= ry1 y ry2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)] 
    (assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) 
           (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years [(:birth-year author) (:death-year author)]]
    (if (first years) 
      (str name " (" (apply str (interpose " - " years)) ")")
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " 
       (authors->string (:authors book))))

(defn books->string [books]
  (cond (== (count books) 0) "No books."
        
        (== (count books) 1) (str "1 book. " 
                                  (book->string (first books)) ".")
        
        (>= (count books) 2) 
        (str (count books) " books. " 
             (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter #(contains? (:authors %) author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter #(not (:death-year %)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (or (<= x1 x3 x2) (>= x1 x3 x2))
         (or (<= y1 y3 y2) (>= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[inner1 inner2] inner] 
    (and (contains-point? outer inner1)
         (contains-point? outer inner2))))

(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-authors  (conj (book :authors) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year )))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [v] (get v 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (apply disj [a-set elem])
    (apply conj [a-set elem])))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) 
     (count (set a-seq))))

(defn old-book->new-book [book]
  (let [author-set (set (book :authors))]
    (assoc book :authors author-set))) 

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (let [authors-seq (map :authors books)] 
    (apply clojure.set/union authors-seq)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (author :name)
        birth (author :birth-year)
        death (author :death-year)
        years (if birth (str " (" birth " - " death ")") )]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (let [number (count books)
        books-string (apply str (interpose ". " (map book->string books)))] 
    (cond (= 0 number) "No books." 
          (= 1 number) (str "1 book. " books-string ".") 
          :else (str number " books. " books-string "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [has-name (fn [author] (= name (author :name)) )
        found (filter has-name authors)]
    (if found (first found))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

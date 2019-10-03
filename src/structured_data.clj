(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (first v)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point] 
    (and (<= x1 xp x2) (<= y1 yp y2))))


(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner] 
    (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))
  

(defn alive? [author]
  (nil? (get author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map fnext collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (author :name) 
        birth (author :birth-year) 
        death (author :death-year)]
    (str name 
         (if birth (str " (" birth " - " death ")")))))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (let [title (book :title) 
        authors (authors->string (book :authors))]
    (str title ", written by " authors)))

(defn books->string [books]
  (str 
   (if (= (count books) 0) 
     "No books." 
     (str (count books) 
          (if (= (count books) 1) 
            (str " book. " 
                 (str (apply book->string books)) ".")
            (str " books. " 
                 (clojure.string/join "" (map #(str (book->string %) ". ") books)))))))) 
  

(defn books-by-author [author books]
  (filter #(has-author? %  author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name  %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
